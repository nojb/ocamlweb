(*
   FICHIER DE TEST 

*)

(* $Id$ *)

{
  open Lexing
  open Filename
  open Location
  open Longident
  open Output
  open Printf
  open Asttypes
  open Parsetree

(* account for "(*" *)
let comment_cpt = ref 0

let parse = 3
(* account for "{" *)
let brace_cpt = ref 0

let camlb = Buffer.create 8192

let token_table = ref Stringset.empty


}

(* regular expressions *)
let space = [' ' '\t']

(*s LEX Analyser *)
rule index_lex = parse
  | "rule" | "and" 
	{ (* look for an identifier name *)
	  index_lex_ident lexbuf;
	  index_lex lexbuf;
	}
  | "let" { get_lex_defs lexbuf; index_lex lexbuf; }
  | "\""*
  | "|"
  | "'"   { ignore_regexpr lexbuf; index_lex lexbuf; }
  | "{"   { in_action := true; offset_action := (lexeme_start lexbuf); (* we enter an action *)
	    incr brace_cpt; Buffer.clear camlb; caml_braces lexbuf;
	    index_lex lexbuf; }
  | "(*"  { incr comment_cpt ; ignore_caml_comments lexbuf; index_lex lexbuf; }
  | eof   { () }
  | _     { index_lex lexbuf; }


and index_yacc_token = parse
  | space* identifier space*
      {
	let lxb = from_string(lexeme lexbuf) in
	let token = enleve_blanc_pipe_dp lxb in  (*i skip leading spaces and ':' i*)
	let loc = { Location.loc_start = lexeme_start lexbuf;
                    Location.loc_end = 0; (* BIDON *)
                    Location.loc_ghost = false (* BIDON *) }
	in

	  add_def loc Token token;
	  token_table := Stringset.add token !token_table;
	  index_yacc_token lexbuf; (* return to caller *)
      }
  | "/*"  { ignore_yacc_comments lexbuf; index_yacc_token lexbuf; }
  | "\n"  { ()  }
  | "<"   { index_yacc_type lexbuf; index_yacc_token lexbuf; }
  | eof   { () }
  | _     { index_yacc_token lexbuf }

and enleve_blanc_pipe_dp = parse
  | "|" | ":"
  | space*     { enleve_blanc_pipe_dp lexbuf; }
  | identifier { lexeme lexbuf; }

(* TRAILER *)
{

let cross_yacc f m =
  reset_cross ();
  current_file := f;
  add_module m;
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
    index_yacc lexbuf 

}
