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

{

  open Lexing
  open Output
  open Web

  let skip_header = ref true

  let comment_depth = ref 0

  let parlist = ref ([] : paragraph list)
  let seclist = ref ([] : raw_section list)

  let reset_lexer () =
    comment_depth := 0;
    parlist := [];
    seclist := []

  let new_section () =
    if !parlist <> [] then begin
      seclist := (List.rev !parlist) :: !seclist
    end;
    parlist := []

  let first_char lexbuf = Lexing.lexeme_char lexbuf 0

  let docub = Buffer.create 8192

  let new_doc () = comment_depth := 1; Buffer.clear docub

  let push_doc () =
    if Buffer.length docub > 0 then begin
      parlist := (Documentation (Buffer.contents docub)) :: !parlist;
      Buffer.clear docub
    end

  let codeb = Buffer.create 8192
		
  let push_code () =
    if Buffer.length codeb > 0 then begin
      parlist := (Code (Buffer.contents codeb)) :: !parlist;
      Buffer.clear codeb
    end

}

let space = [' ' '\t']
let space_or_nl = [' ' '\t' '\n']
let character = 
  "'" ( [^ '\\' '\''] | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] 
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] ) "'"
let rcs_keyword =
  "Author" | "Date" | "Header" | "Id" | "Name" | "Locker" | "Log" |
  "RCSfile" | "Revision" | "Source" | "State"

(* to skip the headers *)
rule header = parse
  | "(*"   { comment_depth := 1; skip_comment lexbuf;
	     skip_until_nl lexbuf; header lexbuf }
  | "\n"   { () }
  | space+ { header lexbuf }
  | _      { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1 }
  | eof    { () }

(* inside a module, at the beginning of a line) *)
and implementation = parse
  | space* "(*" '*'* "*)" space* '\n'
           { implementation lexbuf }
  | space* "(*" space_or_nl*
           { new_doc (); documentation lexbuf; implementation lexbuf }
  | space* "(*s" space_or_nl*
           { new_section (); 
	     new_doc (); documentation lexbuf; implementation lexbuf }
  | space* "(*i"
           { ignore lexbuf; skip_until_nl lexbuf; implementation lexbuf }
  | space* "(*c"
           { comment_depth := 1; Buffer.add_string codeb "(*";
	     comment lexbuf; code lexbuf; implementation lexbuf }
  | space* '\n'   
           { implementation lexbuf }
  | _      { Buffer.clear codeb; Buffer.add_char codeb (first_char lexbuf); 
	     code lexbuf; implementation lexbuf }
  | eof    { new_section (); List.rev !seclist }
      
(* inside the documentation part *)
and documentation = parse
  | "(*" { Buffer.add_string docub (Lexing.lexeme lexbuf);
	   incr comment_depth; documentation lexbuf }
  | space* "*)" 
         { decr comment_depth;
           if !comment_depth > 0 then begin
	     Buffer.add_string docub (Lexing.lexeme lexbuf);
	     documentation lexbuf 
	   end else begin
	     skip_until_nl lexbuf;
	     push_doc ()
	   end}
  | "\036" rcs_keyword [^ '$']* "\036"
         { documentation lexbuf }
  | '\n' " * "
         { Buffer.add_string docub "\n "; documentation lexbuf }
  | eof  { push_doc () }
  | _    { Buffer.add_char docub (first_char lexbuf); documentation lexbuf }

(* inside the code part *)
and code = parse
  | '\n' space* '\n' 
         { push_code () }
  | ";;" { push_code (); skip_until_nl lexbuf }
  | eof  { push_code () }
  | "(*" | "(*c"
         { comment_depth := 1; Buffer.add_string codeb "(*";
	   comment lexbuf; code lexbuf }
  | space* "(*i"
         { ignore lexbuf; skip_until_nl lexbuf; code lexbuf }
  | '"'  { Buffer.add_char codeb '"'; code_string lexbuf; code lexbuf }
  | character
         { Buffer.add_string codeb (Lexing.lexeme lexbuf); code lexbuf }
  | _    { Buffer.add_char codeb (first_char lexbuf); code lexbuf }

(* to skip everything until a newline *)
and skip_until_nl = parse
  | '\n' { () }
  | eof  { () }
  | _    { skip_until_nl lexbuf }

(* to read a comment inside a piece of code *)
and comment = parse
  | "(*" | "(*c"
         { Buffer.add_string codeb "(*"; incr comment_depth; comment lexbuf }
  | "*)" { Buffer.add_string codeb "*)"; decr comment_depth;
           if !comment_depth > 0 then comment lexbuf }
  | eof  { () }
  | _    { Buffer.add_char codeb (first_char lexbuf); comment lexbuf }

(* to skip a comment (used by header) *)
and skip_comment = parse
  | "(*" { incr comment_depth; skip_comment lexbuf }
  | "*)" { decr comment_depth;
           if !comment_depth > 0 then skip_comment lexbuf }
  | eof  { () }
  | _    { skip_comment lexbuf }

(* ignored parts, between "(*i" and "i*)" *)
and ignore = parse
  | "i*)" { () }
  | eof   { () }
  | _     { ignore lexbuf }

(* strings in code *)
and code_string = parse
    '"'      { Buffer.add_char codeb '"' }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
             { Buffer.add_string codeb (Lexing.lexeme lexbuf); 
	       code_string lexbuf }
  | eof      { () }
  | _        { Buffer.add_char codeb (first_char lexbuf); code_string lexbuf }

