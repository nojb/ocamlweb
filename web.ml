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

type raw_section = paragraph list

type interf = { 
  interf_file : string;
  interf_name : string;
  interf_contents : raw_section list }

type implem = { 
  implem_file : string;
  implem_name : string;
  implem_contents : raw_section list;
  implem_interf : (raw_section list) option } 

type file = 
  | Implem of implem
  | Interf of interf
  | Other  of string

(* i*)

(*s Options of the engine. *)

let extern_defs = ref false


(*s Construction of the global index. *)

let sec_number = ref 0

let build_code s =
  let buf = Lexing.from_string s in
  let _ = cross_code buf in ()

let build_interf s =
  let buf = Lexing.from_string s in
  let _ = cross_interf buf in ()

let build_paragraph is_mod = function
  | Documentation s -> ()
  | Code s -> if is_mod then build_code s else build_interf s

let build_section is_mod s = 
  incr sec_number;
  cross_new_section !sec_number;
  List.iter (build_paragraph is_mod) s
    
let build_implem imp =
  cross_new_module imp.implem_name;
  begin match imp.implem_interf with
    | None -> ()
    | Some l -> List.iter (build_section false) l
  end;
  List.iter (build_section true) imp.implem_contents

let build_interf inte =
  cross_new_module inte.interf_name;
  List.iter (build_section false) inte.interf_contents

let build_index l =
  List.iter 
    (function 
       | Implem i -> build_implem i 
       | Interf i -> build_interf i
       | Other f -> ())
    l
    

(*s Printing of the index. *)

let alpha_order = (<)

let all_entries () =
  let s = Idmap.fold (fun x _ s -> Stringset.add x s) !used Stringset.empty in
  let s = Idmap.fold (fun x _ s -> Stringset.add x s) !defined s in
  Sort.list alpha_order (Stringset.elements s)

let print_one_entry s =
  let list_in_table t =
    try 
      let l = Whereset.elements (Idmap.find s !t) in
      let l = List.map (fun x -> x.w_section) l in
      Sort.list (<) l
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

let pretty_print_paragraph = function
  | Documentation s -> pretty_print_doc s
  | Code s -> pretty_print_code s

let pretty_print_section s = 
  incr sec_number;
  output_section !sec_number;
  List.iter 
    (function p -> 
       begin_paragraph ();
       pretty_print_paragraph p;
       end_paragraph())
    s
    
let pretty_print_implem imp =
  output_module imp.implem_name;
  begin match imp.implem_interf with
    | None -> ()
    | Some l -> 
	interface_part ();
	List.iter pretty_print_section l;
	code_part ()
  end;
  List.iter pretty_print_section imp.implem_contents

let pretty_print_interf inte =
  output_interface inte.interf_name;
  List.iter pretty_print_section inte.interf_contents

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


