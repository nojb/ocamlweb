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

(* In that module, we concentrate all the printing functions.
   Thus, it will be easy to add another kind of output, HTML for instance,
   by adapting the code of that module, and nothing else. *)

val set_output_to_file : string -> unit
val close_output : unit -> unit

val output_char : char -> unit
val output_string : string -> unit
val output_escaped_char : char -> unit
val output_file : string -> unit

val set_no_doc : bool -> unit
val latex_header : unit -> unit
val latex_trailer : unit -> unit

val indentation : int -> unit
val enter_math : unit -> unit
val leave_math : unit -> unit

val is_keyword : string -> bool
val output_ident : string -> unit
val output_bc : unit -> unit
val output_ec : unit -> unit
val output_latex_special : string -> unit

val begin_paragraph : unit -> unit
val end_paragraph : unit -> unit
val output_section : int -> unit
val output_module : string -> unit
val interface_part : unit -> unit
val code_part : unit -> unit
val output_interface : string -> unit

val reset_pretty : unit -> unit
