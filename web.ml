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

(*i *)

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

type interf = { 
  interf_file : string;
  interf_name : string;
  interf_contents : raw_section list }

type implem = { 
  implem_file : string;
  implem_name : string;
  implem_contents : raw_section list;
  implem_interf : interf option } 

type file = 
  | Implem of implem
  | Interf of interf
  | Other  of string

(* i*)


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

let index_interf inte =
  cross_interf inte.interf_file

let index_implem imp =
  cross_implem imp.implem_file;
  begin match imp.implem_interf with
    | None -> ()
    | Some i -> index_interf i
  end

let index_file = function 
  | Implem i -> index_implem i 
  | Interf i -> index_interf i
  | Other f -> ()

let build_index l = List.iter index_file l


(*s The sections table. *)

module Intmap = Map.Make(struct type t = int let compare = compare end)

let section_table = ref Idmap.empty

let code_table = ref Idmap.empty

let add_in_table table file loc n =
  let l = try Idmap.find file !table with Not_found -> [] in
  table := Idmap.add file ((loc,n)::l) !table

let add_section = add_in_table section_table

let add_code = add_in_table code_table

let find_where w =
  let rec lookup = function
      [] -> raise Not_found
    | (n,s)::r -> if w.w_loc >= n then s else lookup r
  in
  let table = if !web then !section_table else !code_table in
  lookup (Idmap.find w.w_filename table)


(*s Printing of the index. *)

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

let alpha_string s1 s2 = norm_string s1 < norm_string s2

let all_entries () =
  let s = Idmap.fold (fun x _ s -> Stringset.add x s) !used Stringset.empty in
  let s = Idmap.fold (fun x _ s -> Stringset.add x s) !defined s in
  Sort.list alpha_string (Stringset.elements s)

let rec uniquize = function
  | [] | [_] as l -> l
  | x::(y::r as l) -> if x = y then uniquize l else x :: (uniquize l)

let make_code_label_name (f,n) =
  (Filename.basename f) ^ ":code:" ^ (string_of_int n)

let make_sec_label_name (f,n) =
  (Filename.basename f) ^ ":sec:" ^ (string_of_int n)

let print_one_entry s =
  let list_in_table t =
    try 
      let l = Whereset.elements (Idmap.find s !t) in
      let l = List.map (fun w -> w.w_filename,find_where w) l in
      uniquize (Sort.list (fun (_,y) (_,y') -> y<y') l)
    with Not_found -> 
      []
  in
  let def = list_in_table defined
  and use = list_in_table used in
  if !extern_defs || def <> [] then
    if !web then 
      output_index_entry s def use
    else 
      output_raw_index_entry s 
	(List.map make_code_label_name def) 
	(List.map make_code_label_name use) 

let print_index () =
  begin_index ();
  List.iter print_one_entry (all_entries());
  end_index ()


(*s Production of the \LaTeX\ document. *)

let sec_number = ref 0

let code_number = ref 0

let pretty_print_paragraph f = function
  | Documentation s -> 
      pretty_print_doc s
  | Code (l,s) ->
      if not !web then begin 
	incr code_number;
	add_code f l !code_number;
	output_label (make_code_label_name (f,!code_number))
      end;
      pretty_print_code s

let pretty_print_section f s = 
  if !web then begin
    incr sec_number;
    add_section f s.sec_beg !sec_number;
    begin_section ();
    output_label (make_sec_label_name (f,!sec_number))
  end;
  List.iter (pretty_print_paragraph f) s.sec_contents
    
let pretty_print_implem imp =
  output_module imp.implem_name;
  begin match imp.implem_interf with
    | None -> ()
    | Some i -> 
	interface_part ();
	List.iter (pretty_print_section i.interf_file) i.interf_contents;
	code_part ()
  end;
  List.iter (pretty_print_section imp.implem_file) imp.implem_contents

let pretty_print_interf inte =
  output_interface inte.interf_name;
  List.iter (pretty_print_section inte.interf_file) inte.interf_contents

let pretty_print_file = function
  | Implem i -> pretty_print_implem i 
  | Interf i -> pretty_print_interf i
  | Other f -> output_file f


(*s Production of the document. We proceed in three steps:
    \begin{enumerate}
    \item Build the index;
    \item Pretty-print;
    \item Print the index.
    \end{enumerate}
 *)

let produce_document l =
  build_index l;
  sec_number := 0;
  latex_header !latex_options;
  List.iter pretty_print_file l;
  if !index then print_index ();
  latex_trailer ();
  close_output ()


