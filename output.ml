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

let output_file f =
  let ch = open_in f in
  try
    while true do
      Pervasives.output_char !out_channel (input_char ch)
    done
  with End_of_file -> close_in ch

let output_verbatim s =
  (* TODO : vérifier que "!" n'apparaît pas dans [s] *)
  output_string "\\verb!"; output_string s; output_string "!"
  

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

let indentation n =
  leave_math ();
  let space = 0.5 *. (float n) in
  output_string (sprintf "\\ocwnl{%2.2fem}\n" space)

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

      (* ajoutés *)
      "ref"; "not";
      (* types *)
      "string"; "int"; "array"; "unit"; "bool"; "char"; "list"; "option";
      "float"
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

let output_raw_ident s =
  output_string "\\ocwid{"; output_latex_id s; output_string "}"

let output_ident s =
  if is_keyword s then begin
    leave_math (); output_keyword s
  end else begin
    enter_math ();
    if String.length s = 1 then
      output_latex_id s
    else
      output_raw_ident s
  end

let output_escaped_char c = 
  if c = '^' || c = '~' then leave_math();
  match c with
    | '\\' -> output_string "\\symbol{92}"
    | _ -> output_char '\\'; output_char c; output_string "{}"

let output_latex_special = function
    " " -> output_string "~" 
           (* if !math_mode then output_string "~" else output_char ' ' *)

  | "*" -> enter_math (); output_string "\\times{}"
  | "->" -> enter_math (); output_string "\\rightarrow{}"
  | "<-" -> enter_math (); output_string "\\leftarrow{}"
  | "<=" -> enter_math (); output_string "\\le{}"
  | ">=" -> enter_math (); output_string "\\ge{}"
  | "<>" -> enter_math (); output_string "\\not="
  | "[]" -> output_string "[\\,]"
  | s    -> output_string s

(* comments *)

let output_bc () = leave_math (); output_string "\\ocwbc{}"

let output_ec () = leave_math (); output_string "\\ocwec{}"

(* strings *)

let output_bs () = leave_math (); output_string "\\texttt{\""

let output_es () = output_string "\"}"

let output_vspace () = output_string "\\ocwvspace{}"

(* coming back to initial values *)

let reset_output () =
  math_mode := false

(* sectioning commands *)

let output_section n =
  output_string (sprintf "\\ocwsection{%d}\n" n)

let output_module s =
  output_string "\\ocwmodule{";
  output_latex_id s;
  output_string "}"

let interface_part () =
  output_string "\\ocwinterfacepart{}"

let code_part () =
  output_string "\\ocwcodepart{}"

let output_interface s =
  output_string "\\ocwinterface{";
  output_latex_id s;
  output_string "}"

let begin_paragraph () =
  output_string "\\noindent{}"

let end_paragraph () =
  output_string "\n\n\\medskip{}\n"

let begin_index () =
  output_string "\n\n\\ocwbeginindex{}\n"
let end_index () =
  output_string "\n\n\\ocwendindex{}\n"
  
let print_list print sep l = 
  let rec print_rec = function
      [] -> ()
    | [x] -> print x
    | x::r -> print x; sep(); print_rec r
  in
  print_rec l
  
let output_int n = output_string (string_of_int n)

let output_bf_int n = 
  output_string "\\textbf{"; output_int n; output_string "}"

let output_index_entry s def use =
  let sep () = output_string ", " in
  output_string "\\ocwindexentry{";
  output_raw_ident s;
  output_string "}{";
  print_list output_bf_int sep def;
  output_string "}{";
  if def <> [] && use <> [] then output_string ", ";
  print_list output_int sep use;
  output_string "}\n"

