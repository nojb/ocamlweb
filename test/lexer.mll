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
  open Filename
  open Location

let comment_cpt = ref 0

let brace_cpt = ref 0

let camlb = Buffer.create 8192

let token_table = ref Stringset.empty

let rec skip_dollar = function
    "" -> ""
  |str -> 
     let prem = (String.sub str 0 1) in
     let suite = String.sub str 1 ((String.length str)-1) in
       if prem = "$" 
       then " " ^(skip_dollar suite)
       else prem^(skip_dollar suite)

}


(* regular expressions *)
let space = [' ' '\t']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let identifier = (lowercase | uppercase) identchar*


(*s LEX Analyser *)
rule index_lex = parse
  | "rule" | "and" 
	{ (* look for an identifier name *)

	  index_lex_ident lexbuf;
	  index_lex lexbuf;
	}
  | "let" { get_lex_defs lexbuf; index_lex lexbuf; }
  | "\""
  | "|"
  | "'"   { ignore_regexpr lexbuf; index_lex lexbuf; }
  | "{"   { in_action := true; offset_action := (lexeme_start lexbuf); (* we enter an action *)
	    incr brace_cpt; Buffer.clear camlb; caml_braces lexbuf;
	    index_lex lexbuf; }
  | "(*"  { incr comment_cpt ; ignore_caml_comments lexbuf; index_lex lexbuf; }
  | eof   { () }
  | _     { index_lex lexbuf; }
and index_lex_ident = parse
  | "parse" { () }
  | identifier
      {
	(* add to the index *)
	let entry_pt = lexeme lexbuf in

	let loc = { Location.loc_start = lexeme_start lexbuf;
                    Location.loc_end = 0; (* BIDON *)
                    Location.loc_ghost = false (* BIDON *) } in
	  add_def loc LexEP entry_pt;
prerr_endline("add_def  EntryL "^entry_pt);
	index_lex_ident lexbuf;
	  (*s section sa mere *)      }
  | "(*" {  incr comment_cpt ; ignore_caml_comments lexbuf; index_lex_ident lexbuf; }
  | eof  { () }
  | _    { index_lex_ident lexbuf; }


and enleve_blanc_pipe_dp = parse
  | "|" | ":"
  | space*     { enleve_blanc_pipe_dp lexbuf; }
  | identifier { lexeme lexbuf; }

(* TRAILER *)
{

(*s Given all that collecting functions, we can now define two functions
    [cross_implem] and [cross_interf] which respectively compute the 
    cross-references in implementations and interfaces. *)
(* BUG2 : NON CORRIGE, si la premiere section est definie a la main, juste
apres les 2 commentaires ignores, alors pour le module la position 0
implique que le constructeur de l'index ne sait pas a quelle section ca appartient => ZAP 
*)
let wrapper parsing_function traverse_function f m =
  reset_cross ();
  current_file := f;
  add_module m;
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  try
    traverse_function (parsing_function lexbuf);
    close_in c
  with Syntaxerr.Error _ | Syntaxerr.Escape_error | Lexer.Error _ -> begin
    if not !quiet then
      eprintf " ** warning: syntax error while parsing (while making the index of %s)\n" f;
    close_in c
  end


}
