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

(*s The following functions pretty-print the paragraphs of code and
    documentation, respectively. The boolean argument indicates
    whether the given paragraph is the last one for
    [pretty_print_code] or the first one for [pretty_print_doc]. *)

val pretty_print_code : bool -> string -> unit
val pretty_print_doc  : bool -> string -> unit
