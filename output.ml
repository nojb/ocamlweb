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

(*s \textbf{Low level output.} 
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


(*s \textbf{High level output.}
    In this section and the following, we introduce functions which are 
    \LaTeX\ dependent. 
  *)

let output_verbatim s =
  (*i TODO : v�rifier que "!" n'appara�t pas dans [s] i*)
  output_string "\\verb!"; output_string s; output_string "!"

let no_preamble = ref false

let set_no_preamble b = no_preamble := b

let latex_header opt =
  if not !no_preamble then begin
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
  if not !no_preamble then begin
    output_string "\\end{document}\n"
  end


(*s \textbf{Math mode.}
    We keep a boolean, [math_mode], to know if we are currently
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


(*s \textbf{Indentation.}
    An indentation at the beginning of a line of $n$ spaces 
    is produced by [(indentation n)] (used for code only). *)

let indentation n =
  leave_math ();
  let space = 0.5 *. (float n) in
  output_string (sprintf "\\ocwnl{%2.2fem}\n" space)


(*s \textbf{Keywords.}
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
      (* added for lexers *)
      "rule"; "parse"
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


(*s \textbf{Identifiers.}
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

type char_type = Upper | Lower | Symbol

let what_char = function
  | 'A'..'Z' | '\192'..'\214' | '\216'..'\222' -> Upper
  | 'a'..'z' |'\223'..'\246' | '\248'..'\255' | '_' -> Lower
  | _ -> Symbol

let what_is_first_char s =
  if String.length s > 0 then what_char s.[0] else Lower

let output_raw_ident s =
  begin match what_is_first_char s with
    | Upper -> output_string "\\ocwupperid{"
    | Lower -> output_string "\\ocwlowerid{"
    | Symbol -> output_string "\\ocwsymbolid{"
  end;
  output_latex_id s; 
  output_string "}"

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


(*s \textbf{Symbols.}
    Some mathematical symbols are printed in a nice way, in order
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
  | "&" | "&&" ->
            enter_math (); output_string "\\land{}"
  | "or" | "||" ->
            enter_math (); output_string "\\lor{}"
  | "not" -> enter_math (); output_string "\\lnot{}"
  | "[]" -> output_string "[\\,]"
  | s    -> output_string s

let output_greek_letter = function
  | 'a' -> enter_math (); output_string "\\alpha{}"
  | 'b' -> enter_math (); output_string "\\beta{}"
  | 'c' -> enter_math (); output_string "\\gamma{}"
  | 'd' -> enter_math (); output_string "\\delta{}"
  | c   -> output_char '\''; output_char c


(*s \textbf{Comments.} *)

let output_bc () = leave_math (); output_string "\\ocwbc{}"

let output_ec () = leave_math (); output_string "\\ocwec{}"


(*s \textbf{Strings.} *)

let output_bs () = leave_math (); output_string "\\texttt{\""

let output_es () = output_string "\"}"

let output_vspace () = output_string "\\ocwvspace{}"


(*s Reset of the output machine. *)

let reset_output () =
  math_mode := false


(*s \textbf{Sectioning commands.} *)

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


(*s \textbf{Index.}
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
  

(*s \textbf{Index in WEB style.}
    The function [output_index_entry] prints one entry line, given the
    name of the entry, and two lists of pre-formatted sections labels,
    like 1--4,7,10--17, of type [string elem list].
    The first list if printed in bold face (places where the identifier is
    defined) and the second one in roman (places where it is used).
 *)

type 'a elem = Single of 'a | Interval of 'a * 'a

let output_ref r = output_string (sprintf "\\ref{%s}" r)

let output_elem = function
  | Single r -> 
      output_ref r
  | Interval (r1,r2) -> 
      output_ref r1;
      output_string "--";
      output_ref r2

let output_bf_elem n = 
  output_string "\\textbf{"; output_elem n; output_string "}"

let output_index_entry s def use =
  let sep () = output_string ", " in
  output_string "\\ocwwebindexentry{";
  output_raw_ident s;
  output_string "}{";
  print_list output_bf_elem sep def;
  output_string "}{";
  if def <> [] && use <> [] then output_string ", ";
  print_list output_elem sep use;
  output_string "}\n"


(*s \textbf{Index in \LaTeX\ style.}
    When we are not in WEB style, the index in left to \LaTeX, and all
    the work is done by the macro \verb!\ocwrefindexentry!, which takes
    three arguments: the name of the entry and the two lists of labels where
    it is defined and used, respectively.
 *)

let output_raw_index_entry s def use =
  let sep () = output_string "," 
  and sep' () = output_string ", " in
  output_string "\\ocwrefindexentry{";
  output_raw_ident s;
  output_string "}{";
  print_list output_string sep def;
  output_string "}{";
  print_list output_string sep use;
  output_string "}{";
  print_list output_ref sep' def;
  output_string "}{";
  print_list output_ref sep' use;
  output_string "}\n"

let output_label l =
  output_string "\\label{"; output_string l; output_string "}%\n"
