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
  let seclist = ref ([] : section list)
  let declist = ref ([] : decl list)
  let idslist = ref ([] : string list)

  let new_section () =
    parlist := [];
    idslist := []

  let reset_lexer () =
    comment_depth := 0;
    new_section ();
    seclist := [];
    declist := []

  let push_section () =
    if !parlist <> [] then begin
      seclist :=
      { section_ids = !idslist; section_contents = List.rev !parlist } 
      :: !seclist;
      new_section ()
    end

  let first_char lexbuf = Lexing.lexeme_char lexbuf 0

  let docub = Buffer.create 8192
  let codeb = Buffer.create 8192

  let current_indent = ref 0
  let line_indent = ref 0

  let count_spaces s =
    let c = ref 0 in
    for i = 0 to String.length s - 1 do
      if s.[i] = '\t' then
	c := !c + (8 - (!c mod 8))
      else
	incr c
    done;
    !c

  let beginning_of_line s =
    let n = count_spaces s in
    line_indent := n;
    if n <= !current_indent then current_indent := n

}

let space = [' ' '\t']

(* to skip the headers *)
rule header = parse
  | "(*"   { comment_depth := 1; skip_comment lexbuf; header lexbuf }
  | "\n\n" { () }
  | "\n"   { header lexbuf }
  | space+ { header lexbuf }
  | _      { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1 }
  | eof    { () }

(* to `parse' a module (we always start at the beginning of a line) *)
and implementation = parse
  | "(*"   { comment_depth := 1;
	     if !line_indent > !current_indent then begin
	       Buffer.add_string codeb "(*"; comment lexbuf
	     end else begin
	       Buffer.clear docub; 
	       parlist := (documentation lexbuf) :: !parlist
	     end;
	     implementation lexbuf }
  | space+ { beginning_of_line (Lexing.lexeme lexbuf); implementation lexbuf }
  | '\n'   { beginning_of_line ""; implementation lexbuf }
  | _      { Buffer.add_char codeb (first_char lexbuf); 
	     code_until_nl lexbuf; implementation lexbuf }
  | eof    { push_section (); List.rev !seclist }
      
(* the documentation part *)
and documentation = parse
  | "(*" { comment_depth := succ !comment_depth; documentation lexbuf }
  | "*)" { comment_depth := pred !comment_depth;
           if !comment_depth > 0 then 
	     documentation lexbuf 
	   else begin
	     skip_until_nl lexbuf;
	     Documentation (Buffer.contents docub) 
	   end}
  | "$Id\:" [^ '$']* "$"
         { documentation lexbuf }
  | '\n' " * "
         { Buffer.add_string docub "\n "; documentation lexbuf }
  | _    { Buffer.add_char docub (first_char lexbuf); documentation lexbuf }
  | eof  { Documentation (Buffer.contents docub) }

(* to `parse' an interface *)
and interface = parse
  | eof  { !declist }

(* to skip everything until a newline *)
and skip_until_nl = parse
  | '\n' { () }
  | eof  { () }
  | _    { skip_until_nl lexbuf }

(* to read the code until the newline *)
and code_until_nl = parse
  | '\n' { Buffer.add_char codeb '\n' }
  | "(*" { comment_depth := 1; Buffer.add_string codeb "(*";
	   comment lexbuf; code_until_nl lexbuf }
  | eof  { () }
  | _    { Buffer.add_char codeb (first_char lexbuf); code_until_nl lexbuf }

(* to read a comment inside a piece of code *)
and comment = parse
  | "(*" { Buffer.add_string codeb "(*";
	   comment_depth := succ !comment_depth; comment lexbuf }
  | "*)" { Buffer.add_string codeb "*)";
	   comment_depth := pred !comment_depth;
           if !comment_depth > 0 then 
	     comment lexbuf 
	   else 
	     code_until_nl lexbuf }
  | eof  { () }
  | _    { Buffer.add_char codeb (first_char lexbuf); comment lexbuf }

(* to skip a comment (used by header) *)
and skip_comment = parse
  | "(*" { comment_depth := succ !comment_depth; skip_comment lexbuf }
  | "*)" { comment_depth := pred !comment_depth;
           if !comment_depth > 0 then skip_comment lexbuf }
  | eof  { () }
  | _    { skip_comment lexbuf }
