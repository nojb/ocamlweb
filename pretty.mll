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

  let filename = ref ""

  let is_lex_file () = Filename.check_suffix !filename ".mll"

  let is_yacc_file () = Filename.check_suffix !filename ".mly"

  let braces_depth = ref 0

  let in_C_like_comment = ref false

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
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let caml_token =
    "[" | "]" | "[|" | "|]" | "[<" | ">]" | "{" | "}" | "{<" | ">}" | "[]" 
  | "(" | ")" | "or" | "not" | "||" 
let symbol_token =
  caml_token | (symbolchar+)
let character = 
  "'" ( [^ '\\' '\''] | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] 
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] ) "'"
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

(*s Pretty-printing of code.
    The following function pretty-prints some code and assumes that we are
    at the beginning of a line. *)

rule pr_code = parse
  | space* { let n = count_spaces (lexeme lexbuf) in indentation n;
	     pr_code_inside lexbuf; pr_code lexbuf }
  | eof    { leave_math () }

   
(*s That function pretty-prints the code anywhere else. *)

and pr_code_inside = parse
  | '_'  { if is_lex_file () && !braces_depth = 0 then 
	     output_string "\\ocwlexwc"
           else 
	     output_symbol "_";               
           pr_code_inside lexbuf } 
  | "eof" | "rule" | "parse" | "let" | "and"
         { if is_lex_file () && !braces_depth = 0 then begin
             leave_math (); 
             output_string "\\ocwlexkw{";
             output_string (Lexing.lexeme lexbuf);
             output_string "}"
           end else 
	     output_ident (Lexing.lexeme lexbuf);
	   pr_code_inside lexbuf }
  | "%token" | "%start" | "%type" | "%left" | "%right" | "%nonassoc"
         { let full_lexeme = Lexing.lexeme lexbuf in
           let lexeme = 
	     String.sub full_lexeme 1 (String.length full_lexeme - 1) in 
	   if is_yacc_file () && !braces_depth = 0 then begin
             output_symbol "%";
             leave_math (); 
             output_string "\\ocwyacckw{";
             output_string lexeme;
             output_string "}"
           end else begin
             output_symbol "%";
             output_ident lexeme
           end;
	   pr_code_inside lexbuf }	
  | "error"
         { if is_yacc_file () && !braces_depth = 0 then begin
             leave_math (); 
             output_string "\\ocwyacckw{";
             output_string "error";
             output_string "}"
           end else 
	     output_ident "error";
	pr_code_inside lexbuf }	
  | '{'  { incr braces_depth; 
	   output_symbol (lexeme lexbuf); 
	   pr_code_inside lexbuf }
  | '}'  { decr braces_depth;
	   output_symbol (lexeme lexbuf); 
	   pr_code_inside lexbuf }
  | '*'  { if is_lex_file () && !braces_depth = 0 then begin
             enter_math (); 
             output_string "^\\star{}"
           end else 
	     output_symbol "*";
	   pr_code_inside lexbuf } 
  | '+'  { if is_lex_file () && !braces_depth = 0 then begin
             enter_math (); 
             output_string "^{\\scriptscriptstyle +}"
           end else 
	     output_symbol "+";
	   pr_code_inside lexbuf } 
  | "/*"
      { if is_yacc_file () && !braces_depth = 0 then 
	  in_C_like_comment := true
	else begin
          output_symbol "/";
          output_string "\\star{}"
        end;
	    pr_comment lexbuf; 
	    pr_code_inside lexbuf 
      }	
  | '\n' 
      { end_line () }
  | space+
      { output_char '~'; pr_code_inside lexbuf }
  | character
      { output_verbatim (lexeme lexbuf); pr_code_inside lexbuf }
  | "'" identifier
      { let id = lexeme lexbuf in
	output_type_variable (String.sub id 1 (pred (String.length id))); 
	pr_code_inside lexbuf }
  | "(*"   (*i comment for emacs font-lock mode i*) 
      { output_bc (); comment_depth := 1;
	pr_comment lexbuf; pr_code_inside lexbuf }
  | "(*r" (*i comment for emacs font-lock mode i*) 
      { output_hfill (); output_bc (); comment_depth := 1;
	pr_comment lexbuf; pr_code_inside lexbuf }
  | '"'  
      { output_bs (); pr_code_string lexbuf; pr_code_inside lexbuf }
  | symbol_token
      { output_symbol (lexeme lexbuf); pr_code_inside lexbuf }
  | (identifier '.')* identifier
      { output_ident (lexeme lexbuf); pr_code_inside lexbuf }
  | eof  
      { end_line() }
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { output_integer (lexeme lexbuf); pr_code_inside lexbuf }
  | float_literal
      { output_float (lexeme lexbuf); pr_code_inside lexbuf }
  | _ 
      { output_escaped_char (first_char lexbuf); pr_code_inside lexbuf }


(*s Comments. *)

and pr_comment = parse
  | "/*" { output_symbol "/";
           enter_math (); output_string "\\star{}"; leave_math ();
           pr_comment lexbuf } 
  | "*/" { if !in_C_like_comment then 
	     in_C_like_comment := false
           else begin
             enter_math (); output_string "\\star{}"; leave_math ();
             output_symbol "/";
             pr_comment lexbuf
           end } 
  | "(*" { output_bc (); 
           if not !in_C_like_comment then incr comment_depth; 
	   pr_comment lexbuf }
  | "*)" { output_ec (); 
	   if !in_C_like_comment then 
	     pr_comment lexbuf
           else begin
             decr comment_depth;
             if !comment_depth > 0 then pr_comment lexbuf
           end }
  | '\n' space* '*' ' '
         { output_string "\n "; pr_comment lexbuf }
  | '['  { if !user_math_mode then 
	     output_char '['
	   else begin
	     bracket_depth := 1; escaped_code lexbuf
	   end; 
	   pr_comment lexbuf }
  | eof  { () }
  | '$'  { user_math(); pr_comment lexbuf }
  | _    { output_char (first_char lexbuf); pr_comment lexbuf }


(*s Strings in code. *)

and pr_code_string = parse
  | '"'  { output_es () }
  | '\n' { end_line_string (); pr_code_string lexbuf }
  | ' '  { output_vspace (); pr_code_string lexbuf }
  | '\\' ['"' 't' 'b' 'r']
         { output_escaped_char '\\'; 
	   output_char (lexeme_char lexbuf 1); 
	   pr_code_string lexbuf }
  | '\\' '\n'
         { output_escaped_char '\\'; end_line_string ();
	   pr_code_string lexbuf }
  | '\\' '\\'
         { output_escaped_char '\\'; output_escaped_char '\\'; 
	   pr_code_string lexbuf }
  | eof  { () }
  | '-'  { output_ascii_char 45; pr_code_string lexbuf }
  | _    { output_escaped_char (first_char lexbuf); pr_code_string lexbuf }


(*s Escaped code. *)

and escaped_code = parse
  | '['  { output_char '['; incr bracket_depth; escaped_code lexbuf }
  | ']'  { decr bracket_depth; 
	   if !bracket_depth > 0 then begin
	     output_char ']'; escaped_code lexbuf
           end else
	     if not !user_math_mode then leave_math () }
  | '"'  { output_bs (); pr_code_string lexbuf; escaped_code lexbuf }
  | space+
         { output_char '~'; escaped_code lexbuf }
  | character
         { output_verbatim (lexeme lexbuf); escaped_code lexbuf }
  | "'" identifier
         { let id = lexeme lexbuf in
	   output_type_variable (String.sub id 1 (pred (String.length id))); 
	   escaped_code lexbuf }
  | symbol_token
         { output_symbol (lexeme lexbuf); escaped_code lexbuf }
  | identifier
         { output_ident (lexeme lexbuf); escaped_code lexbuf }
  | eof  { if not !user_math_mode then leave_math () }
  | decimal_literal | hex_literal | oct_literal | bin_literal
         { output_integer (lexeme lexbuf); escaped_code lexbuf }
  | float_literal
         { output_float (lexeme lexbuf); escaped_code lexbuf }
  | _    { output_escaped_char (first_char lexbuf); escaped_code lexbuf }


(*s Documentation. 
    It is output `as is', except for quotations. *)

and pr_doc = parse
  | '[' { if !user_math_mode then 
	     output_char '['
	   else begin
	     bracket_depth := 1; escaped_code lexbuf
	   end; 
	   pr_doc lexbuf }
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

  let pretty_print_code is_last_paragraph s f = 
    reset_pretty ();
    begin_code_paragraph ();
    filename := f;
    pr_code (Lexing.from_string s);
    end_code_paragraph is_last_paragraph

  let pretty_print_doc is_first_paragraph s = 
    reset_pretty ();
    begin_doc_paragraph is_first_paragraph;
    pr_doc (Lexing.from_string s);
    end_doc_paragraph ()

  let pretty_print_lex_code s = 
    failwith "pretty_print_lex_code not implemented"

  let pretty_print_yacc_code s = 
    failwith "pretty_print_yacc_code not implemented"

(*i*)
}
(*i*)


