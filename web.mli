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

type content = { 
  content_file : string;
  content_name : string;
  content_contents : raw_section list } 

type file = 
  | Implem of content
  | Interf of content
  | Other  of string

val extern_defs : bool ref
val add_latex_option : string -> unit
val index : bool ref
val web : bool ref

val produce_document : file list -> unit
