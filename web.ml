(*
 * ocamlweb - A WEB-like tool for ocaml
 * Copyright (C) 1999 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 *)

(* $Id$ *)

(*i*)
open Filename
open Printf
open Cross
open Output
open Pretty

type paragraph =
  | Documentation of string
  | Code of int * string

type raw_section =  {
  sec_contents : paragraph list;
  sec_beg : int }

type content = { 
  content_file : string;
  content_name : string;
  content_contents : raw_section list } 

type file = 
  | Implem of content
  | Interf of content
  | Other  of string

(*i*)


(*s Options of the engine. *)

let index = ref true

let web = ref true

let extern_defs = ref false

let latex_options = ref ""

let add_latex_option s =
  if !latex_options = "" then 
    latex_options := s
  else
    latex_options := !latex_options ^ "," ^ s


(*s Construction of the global index. *)

let index_file = function 
  | Implem i -> cross_implem i.content_file i.content_name
  | Interf i -> cross_interf i.content_file i.content_name
  | Other _ -> ()

let build_index l = List.iter index_file l


(*s The locations tables. \label{counters} *)

module Smap = Map.Make(struct type t = string let compare = compare end)

let sec_locations = ref Smap.empty
let code_locations = ref Smap.empty

let add_file_loc table file loc =
  let l = try Smap.find file !table with Not_found -> [] in
  table := Smap.add file (loc::l) !table

let add_par_loc =
  let par_counter = ref 0 in
  fun f p -> match p with
    | Code (l,_) -> 
	incr par_counter;
	add_file_loc code_locations f (l,!par_counter)
    | Documentation _ -> ()

let add_sec_loc =
  let sec_counter = ref 0 in
  fun f s ->
    incr sec_counter;
    add_file_loc sec_locations f (s.sec_beg,!sec_counter);
    List.iter (add_par_loc f) s.sec_contents

let add_file_loc it =
  List.iter (add_sec_loc it.content_file) it.content_contents

let locations_for_a_file = function
  | Implem i -> add_file_loc i
  | Interf i -> add_file_loc i
  | Other _ -> ()

let find_where w =
  let rec lookup = function
    | [] -> raise Not_found
    | (l,n)::r -> if w.w_loc >= l then ((w.w_filename,l),n) else lookup r
  in
  let table = if !web then !sec_locations else !code_locations in
  lookup (Smap.find w.w_filename table)


(*s Printing of the index. *)

(*s Alphabetic order for index entries. 
    To sort index entries, we define the following order relation 
    [alpha_string]. It puts symbols first (identifiers that do not begin
    with a letter), and symbols are compared using Caml's generic order 
    relation. For real identifiers, we first normalize them by translating
    lowercase characters to uppercase ones and by removing all the accents, 
    and then we use Caml's comparison.
 *)

let norm_char c = match Char.uppercase c with
  | '\192'..'\198' -> 'A'
  | '\199' -> 'C'
  | '\200'..'\203' -> 'E'
  | '\204'..'\207' -> 'I'
  | '\209' -> 'N'
  | '\210'..'\214' -> 'O'
  | '\217'..'\220' -> 'U'
  | '\221' -> 'Y'
  | c -> c

let norm_string s =
  let u = String.copy s in
  for i = 0 to String.length s - 1 do
    u.[i] <- norm_char s.[i]
  done;
  u

let alpha_string s1 s2 = 
  match what_is_first_char s1, what_is_first_char s2 with
    | Symbol, Symbol -> s1 < s2
    | Symbol, _ -> true
    | _, Symbol -> false
    | _,_ -> norm_string s1 < norm_string s2

let order_entry e1 e2 =
  (alpha_string e1.e_name e2.e_name) || 
  (e1.e_name = e2.e_name && e1.e_type < e2.e_type)

(*s The following function collects all the index entries and sort them
    using [alpha_string], returning a list. *)

module Idset = Set.Make(struct type t = index_entry let compare = compare end)

let all_entries () =
  let s = Idmap.fold (fun x _ s -> Idset.add x s) !used Idset.empty in
  let s = Idmap.fold (fun x _ s -> Idset.add x s) !defined s in
  Sort.list order_entry (Idset.elements s)


(*s When we are in \LaTeX\ style, an index entry only consists in two lists
    of labels, which are treated by the \LaTeX\ macro \verb!\ocwrefindexentry!.
    When we are in WEB style, we can do a bit better, replacing a list
    like 1,2,3,4,7,8,10 by 1--4,7,8,10, as in usual \LaTeX\ indexes.
    The following function [intervals] is used to group together the lists 
    of at least three consecutive integers.
 *)

let intervals l =
  let rec group = function
    | (acc, []) -> List.rev acc
    | (Interval (s1,(_,n2))::acc, (f,n)::rem) when n = succ n2 -> 
	group (Interval (s1,(f,n))::acc, rem)
    | ((Single _)::(Single (f1,n1) as s1)::acc, (f,n)::rem) when n = n1+2 ->
	group (Interval ((f1,n1),(f,n))::acc, rem)
    | (acc, (f,n)::rem) ->
	group ((Single (f,n))::acc, rem)
  in
  group ([],l)

let make_label_name (f,n) = f ^ ":" ^ (string_of_int n)

let label_list l =
  List.map (fun x -> make_label_name (fst x)) l

let elem_map f = function
  | Single x -> Single (f x)
  | Interval (x,y) -> Interval (f x, f y)

let web_list l =
  let l = intervals l in
  List.map (elem_map (fun x -> make_label_name (fst x))) l


(*s Printing one index entry. 
    The function call [(list_in_table id t)] collects all the sections for 
    the identifier [id] in the table [t], using the function [find_where], 
    and sort the result thanks to the counter which was associated to each
    new location (see section~\ref{counters}). It also removes the duplicates
    labels.
  *)

let rec uniquize = function
  | [] | [_] as l -> l
  | x::(y::r as l) -> if x = y then uniquize l else x :: (uniquize l)

let map_succeed_nf f l =
  let rec map = function
    | [] -> []
    | x::l -> try (f x)::(map l) with Not_found -> (map l)
  in
  map l

let list_in_table id t =
  try 
    let l = Whereset.elements (Idmap.find id t) in
    let l = map_succeed_nf find_where l in
    let l = Sort.list (fun x x' -> snd x < snd x') l in
    uniquize l
  with Not_found -> 
    []

let entry_type_name = function
  | Value | Constructor -> ""
  | Field      -> "(field)"
  | Type       -> "(type)"
  | Exception  -> "(exn)"
  | Module     -> "(module)"
  | ModuleType -> "(sig)"
  | Class      -> "(class)"
  | Method     -> "(method)"
  | LexParseRule -> "(camllex parsing rule)"
  | RegExpr      -> "(camllex regexpr)"    
  | YaccNonTerminal -> "(camlyacc non-terminal)"
  | YaccTerminal    -> "(camlyacc token)"

let print_one_entry id =
  let def = list_in_table id !defined
  and use = list_in_table id !used in
  let s = id.e_name in
  let t = entry_type_name id.e_type in
  if !extern_defs || def <> [] then
    if !web then 
      output_index_entry s t (web_list def) (web_list use)
    else 
      output_raw_index_entry s t (label_list def) (label_list use)


(*s Then printing the index is just iterating [print_one_entry] on all the
    index entries, given by [(all_entries())]. *)

let print_index () =
  begin_index ();
  List.iter print_one_entry (all_entries());
  end_index ()


(*s Pretty-printing of the document. *)

let pretty_print_paragraph is_first_paragraph is_last_paragraph f = function
  | Documentation s -> 
      pretty_print_doc is_first_paragraph s
  | Code (l,s) ->
      if l>0 then output_label (make_label_name (f,l));
      pretty_print_code is_last_paragraph s f


let pretty_print_section first f s = 
  if !web then begin_section ();
  if first & s.sec_beg > 0 then output_label (make_label_name (f,0));
  output_label (make_label_name (f,s.sec_beg));
  let rec loop is_first_paragraph = function
    | [] ->
	()
    | [ p ] ->
	pretty_print_paragraph is_first_paragraph true f p
    | p :: rest ->
	pretty_print_paragraph is_first_paragraph false f p;
	loop false rest 
  in
  loop true s.sec_contents
    
let pretty_print_sections f = function
  | [] -> ()
  | s :: r -> 
      pretty_print_section true f s; 
      List.iter (pretty_print_section false f) r

let pretty_print_content output_header content =
  output_header content.content_name;
  pretty_print_sections content.content_file content.content_contents

let pretty_print_file = function
  | Implem i -> pretty_print_content output_module i 
  | Interf i -> pretty_print_content output_interface i
  | Other f -> output_file f


(*s Production of the document. We proceed in three steps:
    \begin{enumerate}
    \item Build the index;
    \item Pretty-print of files;
    \item Printing of the index.
    \end{enumerate}
 *)

let produce_document l =
  List.iter locations_for_a_file l;
  build_index l;
  latex_header !latex_options;
  List.iter pretty_print_file l;
  if !index then print_index ();
  latex_trailer ();
  close_output ()




