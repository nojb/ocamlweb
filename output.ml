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

(*i*)
open Printf
(*i*)

(* Low level output. 
   [out_channel] is a reference on the current output channel.
   It is initialized to the standard output and can be 
   redirect to a file by the function [set_output_to_file]. 
   The function [close_output] closes the output channel if it is a file.
   [output_char], [output_string] and [output_file] are self-explainable.
 *)

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


(* \hrulefill *)

(*s \textbf{High level output.}
    In this section and the following, we introduce functions which are 
    \LaTeX\ dependent. 
  *)

let output_verbatim s =
  (*i TODO : vérifier que "!" n'apparaît pas dans [s] i*)
  output_string "\\verb!"; output_string s; output_string "!"

let nodoc = ref false

let set_no_doc b = nodoc := b

let latex_header opt =
  if not !nodoc then begin
    output_string "\\documentclass[12pt]{article}\n";
    output_string "\\usepackage";
    if opt <> "" then output_string (sprintf "[%s]" opt);
    output_string "{ocamlweb}\n";
    output_string "\\usepackage[latin1]{inputenc}\n";
    output_string "\\usepackage[T1]{fontenc}\n";
    output_string "\\usepackage{fullpage}\n";
    output_string "\\begin{document}\n"
  end

let latex_trailer () =
  if not !nodoc then begin
    output_string "\\end{document}\n"
  end


(*s Math mode. We keep a boolean, [math_mode], to know if we are currently
    already in \TeX\ math mode. The functions [enter_math] and [leave_math]
    inserts \verb!$! if necessary, and switch that boolean.
 *)

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


(*s Indentation at the beginning of a line (for code only) is produced by
    the following function. *)

let indentation n =
  leave_math ();
  let space = 0.5 *. (float n) in
  output_string (sprintf "\\ocwnl{%2.2fem}\n" space)


(*s Keywords.
    Caml keywords and base type are stored in two hash tables, and the two
    functions [is_caml_keyword] and [is_base_type] make the corresponding
    tests.
    The function [output_keyword] prints a keyword, with different macros
    for base types and keywords.
  *)

let build_table l = 
  let h = Hashtbl.create 101 in
  List.iter (fun key ->Hashtbl.add h  key ()) l;
  function s -> try Hashtbl.find h s; true with Not_found -> false

let is_caml_keyword = 
  build_table
    [ "and"; "as";  "assert"; "begin"; "class";
      "constraint"; "do"; "done";  "downto"; "else"; "end"; "exception";
      "external";  "false"; "for";  "fun"; "function";  "functor"; "if";
      "in"; "include"; "inherit"; "initializer"; "lazy"; "let"; "match";
      "method";  "module";  "mutable";  "new"; "object";  "of";  "open";
      "or"; "parser";  "private"; "rec"; "sig";  "struct"; "then"; "to";
      "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
      "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr";
      (* ajoutés *)
      "not"
    ]

let is_base_type = 
  build_table
    [ "string"; "int"; "array"; "unit"; "bool"; "char"; "list"; "option";
      "float"; "ref" ]

let is_keyword s = is_base_type s || is_caml_keyword s 

let output_keyword s =
  if is_base_type s then 
    output_string "\\ocwbt{" 
  else 
    output_string "\\ocwkw{";
  output_string s;
  output_string "}"


(*s Identifiers.
    The function [output_raw_ident] prints an identifier,
    escaping the \TeX\ reserved characters with [output_escaped_char].
    The function [output_ident] prints an identifier, calling 
    [output_keyword] if necessary.
 *)

let output_escaped_char c = 
  if c = '^' || c = '~' then leave_math();
  match c with
    | '\\' -> 
	output_string "\\symbol{92}"
    | '$' | '#' | '%' | '&' | '{' | '}' | '^' | '_' | '~' -> 
	output_char '\\'; output_char c; output_string "{}"
    | _ -> 
	output_char c

let output_latex_id s =
  for i = 0 to String.length s - 1 do
    output_escaped_char s.[i]
  done

let output_raw_ident s =
  output_string "\\ocwid{"; output_latex_id s; output_string "}"

let output_ident s =
  if is_keyword s then begin
    leave_math (); output_keyword s
  end else begin
    enter_math ();
    if String.length s = 1 then
      output_escaped_char s.[0]
    else
      output_raw_ident s
  end


(*s Symbols. Some mathematical symbols are printed in a nice way, in order
    to get a more readable code.
    The type variables from \verb!'a! to \verb!'d! are printed as Greek
    letters for the same reason.
 *)

let output_latex_special = function
  | " " -> output_string "~" 

  | "*" -> enter_math (); output_string "\\times{}"
  | "->" -> enter_math (); output_string "\\rightarrow{}"
  | "<-" -> enter_math (); output_string "\\leftarrow{}"
  | "<=" -> enter_math (); output_string "\\le{}"
  | ">=" -> enter_math (); output_string "\\ge{}"
  | "<>" -> enter_math (); output_string "\\not="
  | "(" | ")" as s -> 
            enter_math (); output_string s
  | "[]" -> output_string "[\\,]"
  | s    -> output_string s

let output_greek_letter = function
  | 'a' -> enter_math (); output_string "\\alpha{}"
  | 'b' -> enter_math (); output_string "\\beta{}"
  | 'c' -> enter_math (); output_string "\\gamma{}"
  | 'd' -> enter_math (); output_string "\\delta{}"
  | c   -> output_char '\''; output_char c


(*s Comments. *)

let output_bc () = leave_math (); output_string "\\ocwbc{}"

let output_ec () = leave_math (); output_string "\\ocwec{}"


(*s Strings. *)

let output_bs () = leave_math (); output_string "\\texttt{\""

let output_es () = output_string "\"}"

let output_vspace () = output_string "\\ocwvspace{}"


(*s Reset of the output machine. *)

let reset_output () =
  math_mode := false


(*s Sectioning commands. *)

let begin_section () =
  output_string "\\ocwsection\n"

let output_module s =
  output_string "\\ocwmodule{";
  output_latex_id s;
  output_string "}\n"

let interface_part () =
  output_string "\\ocwinterfacepart{}\n"

let code_part () =
  output_string "\\ocwcodepart{}\n"

let output_interface s =
  output_string "\\ocwinterface{";
  output_latex_id s;
  output_string "}\n"

let begin_paragraph () =
  output_string "\\noindent{}"

let end_paragraph () =
  output_string "\n\n\\medskip{}\n"


(*s Index. 
    It is opened and closed with the two macros \verb!ocwbeginindex! and
    \verb!ocwendindex!.
    The auxiliary function [print_list] is a generic function to print a 
    list with a given printing function and a given separator.
  *)

let begin_index () =
  output_string "\n\n\\ocwbeginindex{}\n"

let end_index () =
  output_string "\n\n\\ocwendindex{}\n"
  
let print_list print sep l = 
  let rec print_rec = function
    | [] -> ()
    | [x] -> print x
    | x::r -> print x; sep(); print_rec r
  in
  print_rec l
  

(*s Index in WEB style. 
    When we are in WEB style, the index is directly printed by the following
    code, and consists in two lists of sections.
    To get a nice output, we want a list like 1,2,3,4,7,8,10 to appear
    as 1--4,7,8,10, as in usual \LaTeX\ indexes.
    The following function [intervals] is used to group together the lists 
    of at least three consecutive integers.
 *)

type elem =
  | Single of string * int
  | Interval of (string * int) * (string * int)

let intervals l =
  let rec group = function
    | (acc, []) -> List.rev acc
    | (Interval (s1,(_,n2))::acc, (f,n)::rem) when n = succ n2 -> 
	group (Interval (s1,(f,n))::acc, rem)
    | ((Single _)::(Single (f1,n1) as s1)::acc, (f,n)::rem) when n = n1+2 ->
	group (Interval ((f1,n1),(f,n))::acc, rem)
    | (acc, (f,n)::rem) ->
	group ((Single (f,n))::acc, rem)
  in
  group ([],l)


(*s Then we can output the index entry, the first sections (where the 
    identifier is defined) in bold face.
 *)

let output_sec_ref (f,n) = output_string (sprintf "\\ref{%s:%d}" f n)

let output_elem = function
  | Single (f,n) -> 
      output_sec_ref (f,n)
  | Interval (s1,s2) -> 
      output_sec_ref s1;
      output_string "--";
      output_sec_ref s2

let output_bf_elem n = 
  output_string "\\textbf{"; output_elem n; output_string "}"

let output_index_entry s def use =
  let sep () = output_string ", " in
  output_string "\\ocwwebindexentry{";
  output_raw_ident s;
  output_string "}{";
  print_list output_bf_elem sep (intervals def);
  output_string "}{";
  if def <> [] && use <> [] then output_string ", ";
  print_list output_elem sep (intervals use);
  output_string "}\n"


(*s Index in \LaTeX\ style. 
    When we are not in WEB style, the index in left to \LaTeX, and all
    the work is done by the macro \verb!ocwrefindexentry!, which takes
    three arguments: the identifier and the two lists of labels where
    it is defined and used respectively.
 *)

let output_raw_index_entry s def use =
  let sep () = output_string "," in
  output_string "\\ocwrefindexentry{";
  output_raw_ident s;
  output_string "}{";
  print_list output_string sep def;
  output_string "}{";
  print_list output_string sep use;
  output_string "}\n"

let output_label l =
  output_string "\\label{"; output_string l; output_string "}%\n"
