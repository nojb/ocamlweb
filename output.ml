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

(* low level output *******************************************************)

let out_channel = ref stdout
let output_is_file = ref false

let set_output_to_file f = 
  out_channel := open_out f;
  output_is_file := true

let close_output () =
  if !output_is_file then close_out !out_channel

let output_char c = Pervasives.output_char !out_channel c

let output_string s = Pervasives.output_string !out_channel s


(* high level output (LaTeX) ************************************************)

let nodoc = ref false

let set_no_doc b = nodoc := b

let latex_header () =
  if not !nodoc then begin
    output_string "\\documentclass[12pt]{article}\n";
    output_string "\\usepackage{ocamlweb}\n";
    output_string "\\usepackage[latin1]{inputenc}\n";
    output_string "\\usepackage[T1]{fontenc}\n";
    output_string "\\usepackage{fullpage}\n";
    output_string "\\begin{document}\n"
  end

let latex_trailer () =
  if not !nodoc then begin
    output_string "\\end{document}\n"
  end

(* indentation *)

let math_mode = ref false
		  
let enter_math () =
  if not !math_mode then begin
    output_string "$";
    math_mode := true
  end

let leave_math () =
  if !math_mode then begin
    output_string "$";
    math_mode := false
  end

let first_line = ref true

let indentation n =
  leave_math ();
  if not !first_line then
    let space = 0.75 *. (float n) in
    output_string (sprintf "\\ocwnl{%2.2fem}\n" space)
  else
    first_line := false

(* keywords and identifiers *)

let is_keyword = 
  let h = Hashtbl.create 101 in
  List.iter
    (fun key ->Hashtbl.add h  key ()) 
    [ "and"; "as";  "assert"; "begin"; "class";
      "constraint"; "do"; "done";  "downto"; "else"; "end"; "exception";
      "external";  "false"; "for";  "fun"; "function";  "functor"; "if";
      "in"; "include"; "inherit"; "initializer"; "lazy"; "let"; "match";
      "method";  "module";  "mutable";  "new"; "object";  "of";  "open";
      "or"; "parser";  "private"; "rec"; "sig";  "struct"; "then"; "to";
      "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";

      "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr";

      "string"; "int"; "array"; "unit"; "bool"
    ];
  function s -> try Hashtbl.find h s; true with Not_found -> false

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
  if is_keyword s then begin
    leave_math (); output_keyword s
  end else begin
    enter_math ();
    if String.length s = 1 then
      output_latex_id s
    else begin
      output_string "\\ocwid{"; output_latex_id s; output_string "}"
    end
  end

let output_latex_special = function
    " " -> output_string "~" 
           (* if !math_mode then output_string "~" else output_char ' ' *)

  | "^" -> output_string "\\^{}"
  | "%" -> output_string "\\%{}"
  | "&" -> output_string "\\&{}"
  | "$" -> output_string "\\${}"
  | "{" -> output_string "\\{{}"
  | "}" -> output_string "\\}{}"

  | "*" -> enter_math (); output_string "\\times{}"
  | "->" -> enter_math (); output_string "\\rightarrow{}"
  | "<-" -> enter_math (); output_string "\\leftarrow{}"
  | "<=" -> enter_math (); output_string "\\le{}"
  | ">=" -> enter_math (); output_string "\\ge{}"
  | "<>" -> enter_math (); output_string "\\not="
  | _   -> assert false

(* comments *)

let output_bc () = leave_math (); output_string "\\ocwbc{}"

let output_ec () = leave_math (); output_string "\\ocwec{}"

(* coming back to initial values *)

let reset_pretty () =
  first_line := true;
  math_mode := false
