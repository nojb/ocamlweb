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

  open Lexing
  open Output
  open Web
(*i*)

  let skip_header = ref true

  let comment_depth = ref 0

  let verb_delim = ref (Char.chr 0)

  let parlist = ref ([] : paragraph list)
  let seclist = ref ([] : raw_section list)

  let section_beg = ref 0

  let new_section () =
    if !parlist <> [] then begin
      let s = { sec_contents = List.rev !parlist; sec_beg = !section_beg } in
      seclist := s :: !seclist
    end;
    parlist := []

  let first_char lexbuf = lexeme_char lexbuf 0

  let docub = Buffer.create 8192

  let new_doc () = comment_depth := 1; Buffer.clear docub

  let push_doc () =
    if Buffer.length docub > 0 then begin
      parlist := (Documentation (Buffer.contents docub)) :: !parlist;
      Buffer.clear docub
    end

  let codeb = Buffer.create 8192

  let code_beg = ref 0
		
  let push_code () =
    if Buffer.length codeb > 0 then begin
      parlist := (Code (!code_beg, Buffer.contents codeb)) :: !parlist;
      Buffer.clear codeb
    end

  let reset_lexer () =
    comment_depth := 0;
    section_beg := 0;
    code_beg := 0;
    parlist := [];
    seclist := []

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
           { new_section (); section_beg := (lexeme_start lexbuf);
	     new_doc (); documentation lexbuf; implementation lexbuf }
  | space* "(*i"
           { ignore lexbuf; skip_until_nl lexbuf; implementation lexbuf }
  | space* "(*c"
           { comment_depth := 1; Buffer.add_string codeb "(*";
	     comment lexbuf; code lexbuf; implementation lexbuf }
  | space* '\n'   
           { implementation lexbuf }
  | _      { Buffer.clear codeb; code_beg := (lexeme_start lexbuf);
	     Buffer.add_char codeb (first_char lexbuf); 
	     code lexbuf; implementation lexbuf }
  | eof    { new_section (); List.rev !seclist }
      
(* inside the documentation part *)
and documentation = parse
  | "(*" { Buffer.add_string docub (lexeme lexbuf);
	   incr comment_depth; documentation lexbuf }
  | space* "*)" 
         { decr comment_depth;
           if !comment_depth > 0 then begin
	     Buffer.add_string docub (lexeme lexbuf);
	     documentation lexbuf 
	   end else begin
	     skip_until_nl lexbuf;
	     push_doc ()
	   end}
  | "\036" rcs_keyword [^ '$']* "\036"
         { documentation lexbuf }
  | "\\verb" _  
         { verb_delim := lexeme_char lexbuf 5;
           Buffer.add_string docub (lexeme lexbuf);
	   inverb lexbuf; documentation lexbuf }
  | "\\begin{verbatim}"
         { Buffer.add_string docub (lexeme lexbuf);
	   inverbatim lexbuf; documentation lexbuf }
  | '\n' " * "
         { Buffer.add_string docub "\n "; documentation lexbuf }
  | eof  { push_doc () }
  | _    { Buffer.add_char docub (first_char lexbuf); documentation lexbuf }

and inverb = parse
  | eof  { () }
  | _    { let c = lexeme_char lexbuf 0 in
	   Buffer.add_char docub c;
           if c == !verb_delim then () else inverb lexbuf }

and inverbatim = parse
  | "\\end{verbatim}"
         { Buffer.add_string docub (lexeme lexbuf) }
  | eof  { () }
  | _    { Buffer.add_char docub (lexeme_char lexbuf 0); inverbatim lexbuf }

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
         { Buffer.add_string codeb (lexeme lexbuf); code lexbuf }
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
  | '"'      { Buffer.add_char codeb '"' }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
             { Buffer.add_string codeb (lexeme lexbuf); 
	       code_string lexbuf }
  | eof      { () }
  | _        { Buffer.add_char codeb (first_char lexbuf); code_string lexbuf }

{

(*s Reading the Caml files. *)

type caml_file = { caml_filename : string; caml_module : string }

let module_name f = String.capitalize (Filename.basename f)

let make_caml_file f = 
  { caml_filename = f;
    caml_module = module_name (Filename.chop_extension f) }

type file_type =
  | File_impl  of caml_file * caml_file option
  | File_intf  of caml_file
  | File_other of string

let raw_read_file f =
  reset_lexer ();
  let c = open_in f in
  let buf = Lexing.from_channel c in
  if !skip_header then header buf;
  let contents = implementation buf in
  close_in c;
  contents

let read_intf i = 
  { interf_file = i.caml_filename; 
    interf_name = i.caml_module; 
    interf_contents = raw_read_file i.caml_filename }
    
let read_impl (m,mi) =
  let interf = match mi with 
    | None -> None
    | Some i -> Some (read_intf i)
  in
  { implem_file = m.caml_filename; 
    implem_name = m.caml_module;
    implem_contents = raw_read_file m.caml_filename;
    implem_interf = interf }

let read_one_file = function
  | File_impl (m,i) -> Implem (read_impl (m,i))
  | File_intf f -> Interf (read_intf f)
  | File_other f -> Other f


}
