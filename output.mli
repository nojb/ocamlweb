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

val set_output_to_file : string -> unit
val close_output : unit -> unit

val output_char : char -> unit
val output_string : string -> unit

val set_no_doc : bool -> unit
val latex_header : unit -> unit
val latex_trailer : unit -> unit

val indentation : int -> unit
val enter_math : unit -> unit
val leave_math : unit -> unit

val output_ident : string -> unit
val output_bc : unit -> unit
val output_ec : unit -> unit
val output_latex_special : string -> unit

val reset_pretty : unit -> unit
