(*
gros blabla
 *)

(* $Id$ *)

(*i*)
{

  open Filename
  open Lexing
  open Output
  open Web
(*i*)

(*s Global variables and functions used by the lexer. *)
let push_caml_subpar () =
  if Buffer.length codeb > 0  
  then 
    begin
      subparlist := (CamlCode (Buffer.contents codeb)) :: !subparlist;
      Buffer.clear codeb;
    end

let reset_lexer () =
  comment_depth := 0;
  section_beg := 0;
  code_beg := 0;
  parlist := [];
  seclist := []

(*i*)
}
(*i*)

(*s Shortcuts for regular expressions. *)

let space = [' ' '\t']
let space_or_nl = [' ' '\t' '\n']

(*s Entry point to skip the headers. Returns when headers are skipped. *)
rule header = parse
  | "(*"   { comment_depth := 1; skip_comment lexbuf;  
	     skip_until_nl lexbuf; header lexbuf } 
  | "\n"   { () } 
  | space+ { header lexbuf } 
  | _      { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1 }
  | eof    { () } 

and caml_subpar = parse
  | space* '\n' space* '\n' 
         { push_caml_subpar () ; push_code () }
  | ";;" { push_caml_subpar () ; push_code (); skip_until_nl lexbuf }
  | eof  { push_caml_subpar () ; push_code () }
  | "(*s"
      {
	new_section (); section_beg := (lexeme_start lexbuf);
	new_doc (); documentation lexbuf; (*i on fait rien => retour au niveau d'appel i*)
      }
  | "(*" | "(*c"
         { comment_depth := 1; Buffer.add_string codeb "(*";
	   comment lexbuf; caml_subpar lexbuf }
  | space* "(*i"
         { caml_ignore lexbuf;   
	   skip_until_nl lexbuf;
	   if !brace_depth <= 0
	   then push_caml_subpar()
	   else caml_subpar lexbuf 
	 }
  | '"'  { Buffer.add_char codeb '"'; code_string lexbuf; caml_subpar lexbuf }
  | '{'  { Buffer.add_char codeb (first_char lexbuf); 
	   incr brace_depth;
	   caml_subpar lexbuf}
  (*i si on a trop d'accolades fermantes c'est qu'on est dans un lex i*)
  | '}'  { decr brace_depth ;  
	   if !brace_depth <= 0
	   then  
	     begin
	       push_caml_subpar ();
	       Buffer.add_char codeb (first_char lexbuf);
	      end
	   else 
	     begin
	       Buffer.add_char codeb (first_char lexbuf); 
	       caml_subpar lexbuf
	     end
	 }
  | character
         { Buffer.add_string codeb (lexeme lexbuf); caml_subpar lexbuf }
  | _    { Buffer.add_char codeb (first_char lexbuf); caml_subpar lexbuf }


(*s Strings in code. *)
and code_string = parse
  | '"'      { Buffer.add_char codeb '"' }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
             { Buffer.add_string codeb (lexeme lexbuf); 
	       code_string lexbuf }
  | eof      { () }
  | _        { Buffer.add_char codeb (first_char lexbuf); code_string lexbuf }

(*i*)
{
(*i*)

(*s \textbf{Caml files.} *)

type caml_file = { caml_filename : string; caml_module : string }

let read_one_file = function
  | File_impl m -> Implem (read m)
  | File_intf m -> Interf (read m)
  | File_lex m -> Lex (read m)
  | File_yacc m -> Yacc (read m)
  | File_other f -> Other f

(*i*)
}
(*i*)
