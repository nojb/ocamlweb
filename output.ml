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

let out_channel = ref stdout
let output_is_file = ref false

let set_output_to_file f = 
  out_channel := open_out f;
  output_is_file := true

let close_output () =
  if !output_is_file then close_out !out_channel


let output_char c = Pervasives.output_char !out_channel c

let output_string s = Pervasives.output_string !out_channel s


let nodoc = ref false

let set_no_doc b = nodoc := b

let latex_header () =
  if not !nodoc then begin
    output_string "\\documentclass[12pt]{article}\n";
    output_string "\\usepackage{ocamlweb}\n";
    output_string "\\usepackage[latin1]{inputenc}\n";
    output_string "\\usepackage[T1]{fontenc}\n";
    output_string "\\begin{document}\n"
  end

let latex_trailer () =
  if not !nodoc then begin
    output_string "\\end{document}\n"
  end

let output_keyword s =
  output_string "\\ocwkw{"; output_string s; output_string "}"

let output_latex_id s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c = '_' then
      output_string "\\_"
    else
      output_char c
  done

let output_ident s =
  output_string "\\ocwid{"; output_latex_id s; output_string "}"
