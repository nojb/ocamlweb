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

type implem = 
    { implem_name : string;
      implem_contents : raw_section list }

type decl = { 
  decl_id : string;
  decl_contents : string;
  decl_spec : string }

type interf = { 
  interf_name : string;
  interf_contents : decl list }

type file = 
    Implem of implem
  | Interf of interf
  | Other  of string


(* production of the TeX document *)

let pretty_print_paragraph = function
    Documentation s -> 
      pretty_print_doc s;
      output_string "\n\n"
  | Code s ->
      pretty_print_code s;
      output_string "\n\n"

let sec_number = ref 0

let pretty_print_implem imp =
  let pretty_print_section s = 
    incr sec_number;
    output_string (sprintf "\\ocwsection{%d}\n" !sec_number);
    List.iter 
      (function p -> 
	 pretty_print_paragraph p;
	 output_string "\\medskip")
      s
  in
  List.iter pretty_print_section imp.implem_contents

let produce_document l =
  List.iter 
    (function 
	 Implem i -> pretty_print_implem i 
       | _ -> ())
    l

