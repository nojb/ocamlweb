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
   by adapting the code of that module, and nothing else. 
   Default output is to standard output, but it can be redirected
   to a file with [set_output_to_file]. [close_output] closes the output,
   if it is a file. *)

val set_output_to_file : string -> unit
val close_output : unit -> unit

(* Then we introduce some low level output functions, for characters 
   and strings.
   [(output_file f)] copies the contents of file [f] on the output. 
   [(output_verbatim s)] outputs the string [s] `as is'. *)

val output_char : char -> unit
val output_string : string -> unit
val output_file : string -> unit
val output_verbatim : string -> unit

(*s The following functions are mainly useful for a \LaTeX\ output, but
    will work correctly in other cases. A call to [set_no_doc] suppresses
    the output of the header and trailer. [(indentation n)] introduces an
    indentation of size [n] at the beggining of a line. *)

val set_no_doc : bool -> unit
val latex_header : unit -> unit
val latex_trailer : unit -> unit

val indentation : int -> unit
val enter_math : unit -> unit
val leave_math : unit -> unit

(*s The following functions are used to pretty-print the code.
    [is_keyword] identifies the keywords of Objective Caml. 
    [output_ident] outputs an identifier, in different faces for keywords 
    and other identifiers, escaping the characters that need it, 
    like _ for instance in \LaTeX. 
    [output_escaped_char] pretty-prints the reserved char of \LaTeX, 
    like \verb!&! or \verb!$!.
    [output_latex_special] pretty-prints some mathematical symbols,
    like $\rightarrow$ for \texttt{->}. *)

val is_keyword : string -> bool
val output_ident : string -> unit
val output_escaped_char : char -> unit
val output_latex_special : string -> unit

(* Comments inside code are opened and closed respectively by 
   [output_bc] and [output_ec]. *)

val output_bc : unit -> unit
val output_ec : unit -> unit

(* Strings inside code are opened and close respectively by
   [ output_bs] and [output_es]. A space character in a string is 
   output as a visible space, with [output_vspace]. *)

val output_bs : unit -> unit
val output_es : unit -> unit
val output_vspace : unit -> unit

(*s The following functions deal with sectioning. The highest level is
    the one of modules and interfaces. A module may contain two subparts, 
    one for the specification and one for the code.
    The next level is the one of section. The last level is the one
    of paragraphs, which are atomic pieces of documentation or code. *)

val output_module : string -> unit
val output_interface : string -> unit

val interface_part : unit -> unit
val code_part : unit -> unit

val output_section : int -> unit
val begin_paragraph : unit -> unit
val end_paragraph : unit -> unit

(*s Index functions. [(output_index_entry f def use)] outputs an entry line
    for function [f], where [def] is the list of sections where [f] is
    introduced and [use] the list of sections where [f] is used. *)

val begin_index : unit -> unit
val output_index_entry : string -> int list -> int list -> unit
val end_index : unit -> unit

(*s The parameters of the output engine are reset to their initial values
    with [reset_output]. *)

val reset_output : unit -> unit
