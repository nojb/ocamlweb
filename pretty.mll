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
{

  open Printf
  open Output
  open Lexing
(*i*)

(*s Global variables and functions used by the lexer. *)

  let comment_depth = ref 0

  let bracket_depth = ref 0

  let verb_delim = ref (Char.chr 0)

  let first_char lexbuf = lexeme_char lexbuf 0

  let count_spaces s =
    let c = ref 0 in
    for i = 0 to String.length s - 1 do
      if s.[i] = '\t' then
	c := !c + (8 - (!c mod 8))
      else
	incr c
    done;
    !c

  let user_math_mode = ref false

  let user_math () =
    if not !user_math_mode then begin
      user_math_mode := true;
      enter_math ()
    end else begin
      user_math_mode := false;
      leave_math ()
    end

  let check_user_math c =
    if !user_math_mode then output_char c else output_escaped_char c

  let reset_pretty () =
    reset_output ();
    user_math_mode := false

(*i*)
}
(*i*)

(*s Shortcuts for regular expressions. *)

let space = [' ' '\t']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let identifier = (lowercase | uppercase) identchar*
let tex_reserved = 
  '$' | '#' | '%' | '&' | '\\' | '{' | '}' | '^' | '_' | '~'
let latex_special = 
  ' ' | "*" | "->" | "<-" | "<=" | ">=" | "<>" | "[]" | "(" | ")" |
  "&" | "&&" | "or" | "||" | "not"
let character = 
  "'" ( [^ '\\' '\''] | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] 
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] ) "'"


(*s Pretty-printing of code.
    The following function pretty-prints some code and assumes that we are
    at the beginning of a line. *)

rule pr_code = parse
  | space* { let n = count_spaces (lexeme lexbuf) in indentation n;
	     pr_code_inside lexbuf; pr_code lexbuf }
  | eof    { leave_math () }

   
(*s That function pretty-prints the code anywhere else. *)

and pr_code_inside = parse
  | '\n' { end_line () }
  | "'" identifier
         { let id = lexeme lexbuf in
	   output_type_variable (String.sub id 1 (pred (String.length id))); 
	   pr_code_inside lexbuf }
  | "(*" { output_bc (); comment_depth := 1;
	   pr_comment lexbuf; pr_code_inside lexbuf }
  | '"'  { output_bs (); pr_code_string lexbuf; pr_code_inside lexbuf }
  | character
         { output_verbatim (lexeme lexbuf); pr_code_inside lexbuf }
  | tex_reserved
         { output_escaped_char (first_char lexbuf); pr_code_inside lexbuf }
  | latex_special  
         { output_latex_special (lexeme lexbuf); pr_code_inside lexbuf }
  | (identifier '.')* identifier
         { output_ident (lexeme lexbuf); pr_code_inside lexbuf }
  | eof  { end_line() }
  | _    { output_char (first_char lexbuf); pr_code_inside lexbuf }


(*s Comments. *)

and pr_comment = parse
  | "(*" { output_bc (); incr comment_depth; pr_comment lexbuf }
  | "*)" { output_ec (); decr comment_depth;
           if !comment_depth > 0 then pr_comment lexbuf }
  | '\n' space* '*' ' '
         { output_string "\n "; pr_comment lexbuf }
  | '['  { bracket_depth := 1; escaped_code lexbuf; pr_comment lexbuf }
  | eof  { () }
  | '$'  { user_math(); pr_comment lexbuf }
  | _    { output_char (first_char lexbuf); pr_comment lexbuf }


(*s Strings in code. *)

and pr_code_string = parse
  | '"'  { output_es () }
  | '\n' { indentation 0; pr_code_string lexbuf }
  | ' '  { output_vspace (); pr_code_string lexbuf }
  | '\\' ['"' 'n' 't' 'b' 'r']
         { output_escaped_char '\\'; 
	   output_char (lexeme_char lexbuf 1); 
	   pr_code_string lexbuf }
  | '\\' '\\'
         { output_escaped_char '\\'; output_escaped_char '\\'; 
	   pr_code_string lexbuf }
  | tex_reserved
         { output_escaped_char (first_char lexbuf); pr_code_string lexbuf }
  | eof  { () }
  | _    { output_char (first_char lexbuf); pr_code_string lexbuf }


(*s Escaped code. *)

and escaped_code = parse
  | '['  { output_char '['; incr bracket_depth; escaped_code lexbuf }
  | ']'  { decr bracket_depth; 
	   if !bracket_depth > 0 then begin
	     output_char ']'; escaped_code lexbuf
           end else
	     if not !user_math_mode then leave_math () }
  | '"'  { output_bs (); pr_code_string lexbuf; escaped_code lexbuf }
  | character
         { output_verbatim (lexeme lexbuf); escaped_code lexbuf }
  | tex_reserved
         { output_escaped_char (first_char lexbuf); escaped_code lexbuf }
  | latex_special  
         { output_latex_special (lexeme lexbuf); escaped_code lexbuf }
  | identifier
         { output_ident (lexeme lexbuf); escaped_code lexbuf }
  | eof  { if not !user_math_mode then leave_math () }
  | _    { output_char (first_char lexbuf); escaped_code lexbuf }


(*s Documentation. 
    It is output `as is', except for quotations. *)

and pr_doc = parse
  | '[' { bracket_depth := 1; escaped_code lexbuf; pr_doc lexbuf }
  | '$' { user_math(); pr_doc lexbuf }
  | "\\verb" _  
         { verb_delim := lexeme_char lexbuf 5;
           output_string (lexeme lexbuf);
	   pr_verb lexbuf; pr_doc lexbuf }
  | "\\begin{verbatim}"
         { output_string (lexeme lexbuf);
	   pr_verbatim lexbuf; pr_doc lexbuf }
  | eof { () }
  | _   { output_char (first_char lexbuf); pr_doc lexbuf }

and pr_verb = parse
  | eof  { () }
  | _    { let c = lexeme_char lexbuf 0 in
	   output_char c;
           if c == !verb_delim then () else pr_verb lexbuf }

and pr_verbatim = parse
  | "\\end{verbatim}"
         { output_string (lexeme lexbuf) }
  | eof  { () }
  | _    { output_char (lexeme_char lexbuf 0); pr_verbatim lexbuf }

(*i*)
{
(*i*)

(*s Then we can introduce two functions [pretty_print_code] and 
    [pretty_print_doc], which pretty-print respectively code and
    documentation parts. 
 *)

  let pretty_print_code s = 
    reset_pretty ();
    begin_code_paragraph ();
    pr_code (Lexing.from_string s);
    end_code_paragraph ()

  let pretty_print_doc s = 
    reset_pretty ();
    begin_doc_paragraph ();
    pr_doc (Lexing.from_string s);
    end_doc_paragraph ()

(*i*)
}
(*i*)
