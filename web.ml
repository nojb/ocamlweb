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

open Printf
open Output
open Pretty

type paragraph =
    Documentation of string
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
    Implem of implem
  | Interf of interf
  | Other  of string


(* production of the TeX document *)

let pretty_print_paragraph = function
    Documentation s -> 
      pretty_print_doc s
  | Code s ->
      pretty_print_code s

let sec_number = ref 0

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
      None -> ()
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
  List.iter 
    (function 
	 Implem i -> pretty_print_implem i 
       | Interf i -> pretty_print_interf i
       | Other f -> output_file f)
    l

