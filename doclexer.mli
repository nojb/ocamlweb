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

(*s Caml files are represented by the record [caml_file], which
    contains their file names and their module names. The functions
    [module_name] and [make_caml_file] are used to construct such
    values. *)

type caml_file = { caml_filename : string; caml_module : string }

val module_name : string -> string

val make_caml_file : string -> caml_file

type file_type =
  | File_impl  of caml_file
  | File_intf  of caml_file
  | File_other of string

(*s The following function [read_one_file] reads a Caml file,
  separating the sections, and separating the paragraphs inside the
  sections. The boolean reference [skip_header] indicates whether the
  header must be skipped. *)

val skip_header : bool ref

val read_one_file : file_type -> Web.file

