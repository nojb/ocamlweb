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

(* This module is the heart of the program. The only function is
   [produce_document], which takes a list of files and produces the
   final \LaTeX\ document. 
   A file is either an implementation, or an interface or 
   any other file, which is then considered as a \LaTeX\ file.
   A file is internally represented by the type [file] defined below,
   which is self-explainable. *)

type paragraph =
  | Documentation of string
  | Code of int * string

type raw_section = {
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

val extern_defs : bool ref
val add_latex_option : string -> unit
val web : bool ref

val produce_document : file list -> unit
