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

(*i $Id$ i*)

{

  open Lex_syntax
  open Yacc_syntax
  open Yacc_parser

(* Auxiliaries for the lexical analyzer *)

let brace_depth = ref 0
and comment_depth = ref 0
and mark_count = ref 0

exception Lexical_error of string * int * int

let line_num = ref 1
let line_start_pos = ref 0

let handle_lexical_error fn lexbuf =
  let line = !line_num
  and column = Lexing.lexeme_start lexbuf - !line_start_pos in
  try
    fn lexbuf
  with Lexical_error(msg, _, _) ->
    raise(Lexical_error(msg, line, column))

(*s yacc keywords *)

let keyword_table = Hashtbl.create 17
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "token", Ttoken;
      "start", Tstart; 
      "type", Ttype;
      "left", Tleft;
      "right", Tright;
      "nonassoc", Tnonassoc;
      "prec", Tprec ]

let keyword_token lexbuf =
  try
    Hashtbl.find keyword_table (Lexing.lexeme lexbuf)
  with
      Not_found ->
	raise(Lexical_error
             ("unknown keyword " ^ String.escaped(Lexing.lexeme lexbuf),
              !line_num, Lexing.lexeme_start lexbuf - !line_start_pos)) 

}

(*s main rule for tokens in yacc files *)

rule main = parse
  | [' ' '\013' '\009' '\012' ] + 
    { main lexbuf }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      main lexbuf }
  | "/*" 
    { handle_lexical_error yacc_comment lexbuf;
      main lexbuf }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
    { match Lexing.lexeme lexbuf with
	  "error" -> Terror
	| s -> Tident(s) }
  | '{' 
    { let n1 = Lexing.lexeme_end lexbuf
      and l1 = !line_num
      and s1 = !line_start_pos in
      brace_depth := 1;
      let n2 = handle_lexical_error action lexbuf in
      Taction({start_pos = n1; end_pos = n2;
               start_line = l1; start_col = n1 - s1}) }
  | '|' 
      { Tor }
  | ';' 
      { Tsemicolon }
  | ':' 
      { Tcolon }
  | '%'  
      { yacc_keyword lexbuf }
  | '<'
      { let n1 = Lexing.lexeme_end lexbuf
	and l1 = !line_num
	and s1 = !line_start_pos in
	let n2 = handle_lexical_error typedecl lexbuf in
	Ttypedecl({start_pos = n1; end_pos = n2;
		   start_line = l1; start_col = n1 - s1}) }
  | eof 
      { EOF }
  | _
    { raise(Lexical_error
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf),
              !line_num, Lexing.lexeme_start lexbuf - !line_start_pos)) }

and yacc_keyword = parse
  | '%' 
      { incr mark_count;
	if !mark_count = 1 then Tmark else 
	  let n1 = Lexing.lexeme_end lexbuf
	  and l1 = !line_num
	  and s1 = !line_start_pos in
	  brace_depth := 0;
	  let n2 = handle_lexical_error action lexbuf in
	  Taction({start_pos = n1; end_pos = n2;
		   start_line = l1; start_col = n1 - s1}) }
  | '{'  
      { let n1 = Lexing.lexeme_end lexbuf
	and l1 = !line_num
	and s1 = !line_start_pos in
	brace_depth := 1;
	let n2 = handle_lexical_error action lexbuf in
	Taction({start_pos = n1; end_pos = n2;
		 start_line = l1; start_col = n1 - s1}) }
  | ['a'-'z']+
      { keyword_token lexbuf }
  | _ 
      { raise(Lexical_error
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf),
              !line_num, Lexing.lexeme_start lexbuf - !line_start_pos)) }


(*s recognizes a CAML action *)
 
and action = parse
  | '{' 
    { incr brace_depth;
      action lexbuf }
  | '}' 
    { decr brace_depth;
      if !brace_depth = 0 then Lexing.lexeme_start lexbuf else action lexbuf }
  | "%}" 
    { decr brace_depth;
      if !brace_depth = 0 then Lexing.lexeme_start lexbuf else 
	raise(Lexical_error
		("ill-balanced brace ",
		 !line_num, Lexing.lexeme_start lexbuf - !line_start_pos)) }
  | '"' 
    { string lexbuf;
      action lexbuf }
  | "'" [^ '\\'] "'" 
    { action lexbuf }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'" 
    { action lexbuf }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'" 
    { action lexbuf }
  | "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }
  | eof 
    { if !brace_depth = 0 then Lexing.lexeme_start lexbuf else
	raise (Lexical_error("unterminated action", 0, 0)) }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      action lexbuf }
  | _ 
    { action lexbuf }

(*s recognizes a CAML ytpe between $<$ and $>$ *)
      
and typedecl = parse
  | '>' 
    { Lexing.lexeme_start lexbuf }
  | eof 
    { raise (Lexical_error("unterminated type declaration", 0, 0)) }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      typedecl lexbuf }
  | "->" 
    { typedecl lexbuf }
  | _ 
    { typedecl lexbuf }
      
and string = parse
    '"' 
    { () }
  | '\\' [' ' '\013' '\009' '\012'] * '\010' [' ' '\013' '\009' '\012'] *
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
    { string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] 
    { string lexbuf }
  | eof 
    { raise(Lexical_error("unterminated string", 0, 0)) }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      string lexbuf }
  | _ 
    { string lexbuf }

and comment = parse
    "(*" 
    { incr comment_depth; comment lexbuf }
  | "*)" 
    { decr comment_depth;
      if !comment_depth = 0 then () else comment lexbuf }
  | '"' 
    { string lexbuf;
      comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof 
    { raise(Lexical_error("unterminated comment", 0, 0)) }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      comment lexbuf }
  | _ 
    { comment lexbuf }

and yacc_comment = parse
  | "*/" 
    { () }
  | eof 
    { raise(Lexical_error("unterminated yacc comment", 0, 0)) }
  | '\010'
    { line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      yacc_comment lexbuf }
  | _ 
    { yacc_comment lexbuf }

