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
  | Code of string

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

let extern_defs = ref false


(*s Construction of the global index. *)

let build_interf inte =
  cross_interf inte.interf_file

let build_implem imp =
  cross_implem imp.implem_file;
  begin match imp.implem_interf with
    | None -> ()
    | Some i -> build_interf i
  end

let build_index l =
  List.iter 
    (function 
       | Implem i -> build_implem i 
       | Interf i -> build_interf i
       | Other f -> ())
    l


(*s The sections' table. *)

module Intmap = Map.Make(struct type t = int let compare = compare end)

let section_table = ref Idmap.empty

let add_section file loc n =
  let l = try Idmap.find file !section_table with Not_found -> [] in
  section_table := Idmap.add file ((loc,n)::l) !section_table

let find_section w =
  let rec lookup = function
      [] -> raise Not_found
    | (n,s)::r -> if w.w_loc >= n then s else lookup r
  in
  lookup (Idmap.find w.w_filename !section_table)


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

let print_one_entry s =
  let list_in_table t =
    try 
      let l = Whereset.elements (Idmap.find s !t) in
      let l = List.map find_section l in
      uniquize (Sort.list (<) l)
    with Not_found -> 
      []
  in
  let def = list_in_table defined
  and use = list_in_table used in
  if !extern_defs || def <> [] then
    output_index_entry s def use

let print_index () =
  begin_index ();
  List.iter print_one_entry (all_entries());
  end_index ()


(*s Production of the \LaTeX\ document. *)

let sec_number = ref 0

let pretty_print_paragraph = function
  | Documentation s -> pretty_print_doc s
  | Code s -> pretty_print_code s

let pretty_print_section f s = 
  incr sec_number;
  add_section f s.sec_beg !sec_number;
  output_section !sec_number;
  List.iter 
    (function p -> 
       begin_paragraph ();
       pretty_print_paragraph p;
       end_paragraph())
    s.sec_contents
    
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


(*s Production of the document. 
    1. Build the index. 2. Pretty-print. 3. Print the index. *)

let produce_document l =
  build_index l;
  sec_number := 0;
  List.iter 
    (function 
       | Implem i -> pretty_print_implem i 
       | Interf i -> pretty_print_interf i
       | Other f -> output_file f)
    l;
  print_index ()


