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

  open Filename
  open Lexing
  open Output
  open Web
(*i*)

(*s Global variables and functions used by the lexer. *)
  let print_actions = ref true

  let skip_header = ref true

  let comment_depth = ref 0
  (*i prof des accolades pour les fichiers lex i*) 
  let brace_depth = ref 0
  (*i prof des % accolades et %% pour les fichiers yacc i*) 
  let dans_caml = ref false
  let cptpc = ref 0  

  let subparlist = ref ([] : sub_paragraph list)
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
    if (!subparlist != []) then
      begin
	parlist := (Code (!code_beg, List.rev !subparlist)) :: !parlist;
	subparlist:=[];
      end

  let push_caml_subpar () =
    if Buffer.length codeb > 0  
    then 
      begin
	subparlist := (CamlCode (Buffer.contents codeb)) :: !subparlist;
	Buffer.clear codeb;
      end

  let push_lex_subpar () =
    if Buffer.length codeb > 0 then 
      begin
	subparlist := (LexCode (Buffer.contents codeb)) :: !subparlist;
	Buffer.clear codeb;
      end

  let push_yacc_subpar () =
    if Buffer.length codeb > 0 then 
      begin
	subparlist := (YaccCode (Buffer.contents codeb)) :: !subparlist;
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
let character = 
  "'" ( [^ '\\' '\''] | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] 
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] ) "'"
let rcs_keyword =
  "Author" | "Date" | "Header" | "Id" | "Name" | "Locker" | "Log" |
  "RCSfile" | "Revision" | "Source" | "State"


(*s Entry point to skip the headers. Returns when headers are skipped. *)
rule header = parse
  | "(*"   { comment_depth := 1; skip_comment lexbuf;  
	     skip_until_nl lexbuf; header lexbuf } 
  | "\n"   { () } 
  | space+ { header lexbuf } 
  | _      { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1 }
  | eof    { () } 

and yacc_header = parse
  | "/*"   { comment_depth := 1; yacc_skip_comment lexbuf;
	     skip_until_nl lexbuf; yacc_header lexbuf } 
  | "\n"   { () } 
  | space+ { yacc_header lexbuf } 
  | _      { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1 }
  | eof    { () } 

(*s Inside a module, at the beginning of a line. *)
and caml_implementation = parse
  | space* "(*" '*'* "*)" space* '\n'
           { caml_implementation lexbuf }
  | space* "(*" space_or_nl*
           { new_doc (); documentation lexbuf; caml_implementation lexbuf }
  | space* "(*s" space_or_nl*
           { new_section (); section_beg := (lexeme_start lexbuf);
	     new_doc (); documentation lexbuf; caml_implementation lexbuf }
  | space* "(*i"
           { caml_ignore lexbuf; 
	     skip_until_nl lexbuf;
	     caml_implementation lexbuf }
  | space* "(*c"
           { comment_depth := 1; Buffer.add_string codeb "(*";
	     comment lexbuf; caml_subpar lexbuf; caml_implementation lexbuf }
  | space* '\n'   
           { caml_implementation lexbuf }
  | _      { Buffer.clear codeb; code_beg := (lexeme_start lexbuf);
	     Buffer.add_char codeb (first_char lexbuf); 
	     caml_subpar lexbuf; caml_implementation lexbuf }
  | eof    { new_section (); List.rev !seclist }
      
(*s Inside a lex module, at the beginning of a line. *)
and lex_implementation = parse
  | space* "(*" '*'* "*)" space* '\n'
           { lex_implementation lexbuf }
  | space* "(*" space_or_nl*  
           { new_doc (); documentation lexbuf; lex_implementation lexbuf }
  | space* "(*s" space_or_nl*
           { new_section (); section_beg := (lexeme_start lexbuf);
	     new_doc (); documentation lexbuf; lex_implementation lexbuf }
  | space* "(*i" 
           { 
	     lex_ignore lexbuf; (*[skip_until_nl lexbuf];*)
	     (if ( !cptpc >= 2 || 
	     (!cptpc = 1 && !brace_depth > 0) ||
	     (!cptpc < 1 && !dans_caml) )
	     then   
	       begin 
		 push_lex_subpar ();
		 caml_subpar lexbuf;   
	       end);
	     lex_implementation lexbuf }
  | space* "(*c"
           { comment_depth := 1; Buffer.add_string codeb "(*";
	     comment lexbuf; lex_subpar lexbuf; lex_implementation lexbuf }
  | space* '\n'    
           { lex_implementation lexbuf }
  | space* "{" 
      {  
	(if !brace_depth > 0 
	then 
	  begin
	    incr brace_depth;
	    caml_subpar lexbuf
	  end
	else
	  begin
	    Buffer.add_char codeb '{';
	    incr brace_depth;
	    push_lex_subpar();
 	    caml_subpar lexbuf;
	  end);
	  lex_implementation lexbuf;   
      } 
  | space* "}" 
      {
	decr brace_depth;
	if !brace_depth <= 0 
	then 
	  begin
	    Buffer.add_char codeb '}';
	    lex_subpar lexbuf
	  end
	else  
	    caml_subpar lexbuf;
	lex_implementation lexbuf; 
      }
  | _      
      { 
	if !brace_depth <= 0 
	then
	  begin
 	    Buffer.clear codeb; 
	    code_beg := (lexeme_start lexbuf);
	    Buffer.add_char codeb (first_char lexbuf); 
	    lex_subpar lexbuf
	  end
	 else
	  begin
 	    Buffer.clear codeb; 
	    code_beg := (lexeme_start lexbuf);
	    Buffer.add_char codeb (first_char lexbuf); 
	    caml_subpar lexbuf
	  end;
	  lex_implementation lexbuf 
      }
  | eof    { new_section (); List.rev !seclist }

(*s Inside a yacc module, at the beginning of a line. *)
and yacc_implementation = parse
  | space* "/*" '*'* "*/" space* '\n'
           { yacc_implementation lexbuf }
  | space* "/*s" space_or_nl*
           { new_section (); section_beg := (lexeme_start lexbuf);
	     new_doc (); yacc_documentation lexbuf; yacc_implementation lexbuf }
  | space* "/*i"
           { 
	     yacc_ignore lexbuf; (*[skip_until_nl lexbuf];*)
	     (if ( !cptpc >= 2 || 
	     (!cptpc = 1 && !brace_depth > 0) ||
	     (!cptpc < 1 && !dans_caml) )
	     then   
	       begin 
		 push_yacc_subpar ();
		 caml_subpar lexbuf;   
	       end);
	     yacc_implementation lexbuf }
  | space* "/*c"
      { comment_depth := 1; Buffer.add_string codeb "/*";
	yacc_comment lexbuf; yacc_subpar lexbuf; yacc_implementation lexbuf }
  | space* "/*" space_or_nl*
           { new_doc (); yacc_documentation lexbuf; yacc_implementation lexbuf }
  | space* '\n'   
      { yacc_implementation lexbuf }
  | space* "%{" 
      {  
	Buffer.add_string codeb (lexeme lexbuf);
	dans_caml := true;
	(if ( !cptpc >= 2 || 
	      (!cptpc = 1 && !brace_depth > 0) ||
	      (!cptpc < 1 && !dans_caml))
	 then 
	   begin
	     push_yacc_subpar();
 	     caml_subpar lexbuf;
	   end
	 else
	   begin
	     caml_subpar lexbuf;
	   end);
	yacc_implementation lexbuf;   
      } 
  | space* "%}" 
      {
	dans_caml := false;
	Buffer.add_string codeb (lexeme lexbuf) ;
	(if ( !cptpc >= 2 || 
	     (!cptpc = 1 && !brace_depth > 0) ||
	     (!cptpc < 1 && !dans_caml) )
	then 
	  caml_subpar lexbuf
	else  
	  begin
	    yacc_subpar lexbuf;
	  end);
	  yacc_implementation lexbuf; 
      }
 
  | space* "{" 
      {  
	Buffer.add_string codeb (lexeme lexbuf);
	incr brace_depth;
	(if ( !cptpc >= 2 || 
	      (!cptpc = 1 && !brace_depth > 0) ||
	      (!cptpc < 1 && !dans_caml) )
	 then 
	   begin
	     caml_subpar lexbuf;
	   end
	 else
	   begin
	     push_yacc_subpar();
 	     caml_subpar lexbuf;
	   end);
	yacc_implementation lexbuf;   
      } 
  | space* "}" 
      {
	Buffer.add_string codeb (lexeme lexbuf) ;
	decr brace_depth;
	(if ( !cptpc >= 2 || 
	      (!cptpc = 1 && !brace_depth > 0) ||
	      (!cptpc < 1 && !dans_caml) ) 
	 then 
	   begin
	     caml_subpar lexbuf;
	   end
	 else  
	   begin
	     yacc_subpar lexbuf;
	   end);
	yacc_implementation lexbuf; 
      }
  | space* "%%"
      {
	Buffer.add_string codeb "%%" ;
	incr cptpc;
        yacc_subpar lexbuf;
	yacc_implementation lexbuf;
      }
  | '<'
      {
	code_beg := (lexeme_start lexbuf);
	Buffer.add_char codeb (first_char lexbuf); 
	special_caml_subpar lexbuf;
        yacc_implementation lexbuf;
      }
  | _      
      { 
	(if ( !cptpc >= 2 || 
	      (!cptpc = 1 && !brace_depth > 0) ||
	      (!cptpc < 1 && !dans_caml) ) 
	 then
	   begin
 	     (*Buffer.clear codeb; ???????????*) 
	     code_beg := (lexeme_start lexbuf);
	     Buffer.add_char codeb (first_char lexbuf); 
	     caml_subpar lexbuf;
	   end
	 else
	   begin
 	     (*Buffer.clear codeb;   ??????????*) 
	     code_beg := (lexeme_start lexbuf);
	     Buffer.add_char codeb (first_char lexbuf); 
	     yacc_subpar lexbuf;
	   end);
	yacc_implementation lexbuf;
      }
  | eof    { new_section (); List.rev !seclist; }
      
(*s Inside the documentation part, anywhere. *)
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
  | '\n' " * "
         { Buffer.add_string docub "\n "; documentation lexbuf }
  | eof  { push_doc () }
  | _    { Buffer.add_char docub (first_char lexbuf); documentation lexbuf }


(*s Inside the documentation part in yacc files, anywhere. *)
and yacc_documentation = parse
  | "/*" { Buffer.add_string docub (lexeme lexbuf);
	   incr comment_depth; documentation lexbuf }
  | space* "*/" 
         { decr comment_depth;
           if !comment_depth > 0 then 
	     begin
	       Buffer.add_string docub (lexeme lexbuf);
	       yacc_documentation lexbuf 
	     end 
	   else
	     begin
	       skip_until_nl lexbuf;
	       push_doc ()
	     end}
  | "\036" rcs_keyword [^ '$']* "\036"
         { documentation lexbuf }
  | '\n' " * "
         { Buffer.add_string docub "\n "; yacc_documentation lexbuf }
  | eof  { push_doc () }
  | _    { Buffer.add_char docub (first_char lexbuf); yacc_documentation lexbuf }


(*s Inside the code part, inthe the type declaration of yacc entry points. *)
and special_caml_subpar = parse
  | "->"
      { 
	Buffer.add_string codeb (lexeme lexbuf);
	special_caml_subpar lexbuf ; 
      }
  | ">" 
      { 
	push_caml_subpar ();
	Buffer.add_string codeb (lexeme lexbuf); 
      }
  | "(*"
      {		
	Buffer.add_string codeb (lexeme lexbuf); 
	comment lexbuf ;
	special_caml_subpar lexbuf;
      }
  | _  
      { 
	Buffer.add_char codeb (first_char lexbuf);
	special_caml_subpar lexbuf;
      }
    

(*s Inside the code part, anywhere. *)
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

and lex_subpar = parse
  | space* '\n' space* '\n' 
         { push_lex_subpar () ; push_code () }
  | ";;" { push_lex_subpar () ; push_code (); skip_until_nl lexbuf }
  | eof  { push_lex_subpar () ; push_code () }
  | "(*s"
      {
	new_section (); section_beg := (lexeme_start lexbuf);
	new_doc (); documentation lexbuf; (*i on fait rien => retour au niveau d'appel i*)
      }
  | "(*" | "(*c"
         { comment_depth := 1; Buffer.add_string codeb "(*"; 
	   comment lexbuf; lex_subpar lexbuf } 
  | space* "(*i"
         { lex_ignore lexbuf; skip_until_nl lexbuf;
	   (if !brace_depth > 0
	   then   
	     begin 
	       push_lex_subpar ();
	       caml_subpar lexbuf  
	     end); 
	   lex_subpar lexbuf }
  | '"'  { Buffer.add_char codeb '"'; 
	   code_string lexbuf; 
	   lex_subpar lexbuf } 
  | '{'  { Buffer.add_char codeb (first_char lexbuf) ; 
	   incr brace_depth; 
	   push_lex_subpar () ;  
	   caml_subpar lexbuf ;  
	   lex_subpar lexbuf }
  | '}'  { Buffer.add_char codeb (first_char lexbuf) ; 
	   decr brace_depth; 
	   lex_subpar lexbuf } 
  | character  
         { Buffer.add_string codeb (lexeme lexbuf); lex_subpar lexbuf }
  | _    { Buffer.add_char codeb (first_char lexbuf); lex_subpar lexbuf }


and yacc_subpar = parse 
  | space* '\n' space* '\n'  
         { push_yacc_subpar () ; push_code (); } 
  | ";;" { push_yacc_subpar () ; push_code (); (*[skip_until_nl lexbuf]*) }
  | eof  { push_yacc_subpar () ; push_code (); } 
  | "/*s"
      {
	new_section (); section_beg := (lexeme_start lexbuf);
	new_doc (); yacc_documentation lexbuf; (*i on fait rien => retour au niveau d'appel i*)
      }
  | "/*" | "/*c"
        { comment_depth := 1; Buffer.add_string codeb "/*";
	  yacc_comment lexbuf; yacc_subpar lexbuf; }
  | space* "/*i"
      { yacc_ignore lexbuf; skip_until_nl lexbuf;
	(if !brace_depth > 0
	then   
	  begin 
	    push_yacc_subpar ();
	    caml_subpar lexbuf;  
	  end);
        yacc_subpar lexbuf;
	 } 
  | '"'  { Buffer.add_char codeb '"'; code_string lexbuf; yacc_subpar lexbuf }
  | "%{" { Buffer.add_string codeb (lexeme lexbuf) ;
	   dans_caml := true; 	   
	   push_yacc_subpar () ; 
	   caml_subpar lexbuf ; 
	   yacc_subpar lexbuf; }
  | "%}"  { Buffer.add_string codeb (lexeme lexbuf) ; 
	   dans_caml := false; 
	   yacc_subpar lexbuf } 
  | '{'  { Buffer.add_char codeb (first_char lexbuf) ; 
	   incr brace_depth;
	   push_yacc_subpar () ;  
	   caml_subpar lexbuf ;  
	   yacc_subpar lexbuf }
  | '}'  { Buffer.add_char codeb (first_char lexbuf) ; 
	   decr brace_depth; 
	   yacc_subpar lexbuf; } 
  | space* "%%"
      {
	Buffer.add_string codeb (lexeme lexbuf) ;
        push_yacc_subpar ();
	incr cptpc;
	yacc_subpar lexbuf;
      }
  | '<'
      {
	code_beg := (lexeme_start lexbuf);
	Buffer.add_char codeb (first_char lexbuf);
	push_yacc_subpar ();
	special_caml_subpar lexbuf;
        yacc_subpar lexbuf;
      }
  | character 
      { Buffer.add_string codeb (lexeme lexbuf); yacc_subpar lexbuf; }
  | _    { Buffer.add_char codeb (first_char lexbuf); yacc_subpar lexbuf; }



(*s To skip everything until a newline. *)
and skip_until_nl = parse
  | '\n' { () }
  | eof  { () }
  | _    { skip_until_nl lexbuf }

(*s To read a comment inside a piece of code. *)
and comment = parse
  | "(*" | "(*c"
         { Buffer.add_string codeb "(*"; incr comment_depth; comment lexbuf }
  | "*)" { Buffer.add_string codeb "*)"; decr comment_depth;
           if !comment_depth > 0 then comment lexbuf }
  | eof  { () }
  | _    { Buffer.add_char codeb (first_char lexbuf); comment lexbuf }

(*s To skip a comment (used by [header]). *)
and skip_comment = parse
  | "(*" { incr comment_depth; skip_comment lexbuf }
  | "*)" { decr comment_depth;
           if !comment_depth > 0 then skip_comment lexbuf }
  | eof  { () }
  | _   { skip_comment lexbuf }
(*s To read a comment inside a piece of code. *)
and yacc_comment = parse
  | "/*" | "/*c"
         { Buffer.add_string codeb "/*"; incr comment_depth; comment lexbuf }
  | "*/" { Buffer.add_string codeb "*/"; decr comment_depth;
           if !comment_depth > 0 then yacc_comment lexbuf }
  | eof  { () }
  | _    { Buffer.add_char codeb (first_char lexbuf); yacc_comment lexbuf }

(*s To skip a comment (used by [header]). *)
and yacc_skip_comment = parse
  | "/*" { incr comment_depth; yacc_skip_comment lexbuf }
  | "*/" { decr comment_depth;
           if !comment_depth > 0 then yacc_skip_comment lexbuf }
  | eof  { () }
  | _   { yacc_skip_comment lexbuf }

(*s Ignored parts, between "(*i" and "i*)". Note that such comments
    are not nested. *)
and caml_ignore = parse
  | "i*)" { () }
  | "{"   { incr brace_depth; caml_ignore lexbuf }
  | "}"   { decr brace_depth; caml_ignore lexbuf }
  | eof   { prerr_endline "Unterminated ocamlweb (caml) comment"; exit 1 }
  | _    { caml_ignore lexbuf }

(*s Ignored parts, between "(*i" and "i*)". Note that such comments
    are not nested. *)
and lex_ignore = parse
  | "i*)" { () }
  | "{"   { incr brace_depth; lex_ignore lexbuf }
  | "}"   { decr brace_depth; lex_ignore lexbuf }
  | eof   { prerr_endline "Unterminated ocamlweb (lex) comment"; exit 1 }
  | _     { lex_ignore lexbuf }

(*s Ignored parts, between "(*i" and "i*)". Note that such comments
    are not nested. *)
and yacc_ignore = parse
  | "i*/" { () }
  | "{"   { incr brace_depth; yacc_ignore lexbuf }
  | "}"   { decr brace_depth; yacc_ignore lexbuf }
  | "%{"  { dans_caml := true; yacc_ignore lexbuf }
  | "%}"  { dans_caml := false; yacc_ignore lexbuf }
  | "%%"  { incr cptpc; yacc_ignore lexbuf; }
  | eof   { prerr_endline "Unterminated ocamlweb (yacc) comment"; exit 1 }
  | _    { yacc_ignore lexbuf }


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

let module_name f = String.capitalize (Filename.basename f)

let make_caml_file f = 
  { caml_filename = f;
    caml_module = module_name (Filename.chop_extension f) }

type file_type =
  | File_impl  of caml_file
  | File_intf  of caml_file
  | File_lex   of caml_file
  | File_yacc  of caml_file
  | File_other of string
;;
(*s \textbf{Reading Caml files.} *)

let raw_read_file f =
  reset_lexer ();
  let c = open_in f in
  let buf = Lexing.from_channel c in
    (*i suivant le fichier on appelle le bon lexer i*)
    if check_suffix f ".mll"
    then
      begin
      	if !skip_header then header buf;
    	let contents = lex_implementation buf in
      	  close_in c;
      	  contents
      end
    else 
      if check_suffix f ".mly"
      then
 	begin
          if !skip_header then yacc_header buf;
	  let contents = yacc_implementation buf in
      	    close_in c;
      	    contents
	end
      else 
	begin
    	  if !skip_header then header buf;
      	  let contents = caml_implementation buf in
      	    close_in c;
      	    contents
	end
;;
let read m =
  { content_file = m.caml_filename; 
    content_name = m.caml_module;
    content_contents = raw_read_file m.caml_filename }
;;
let read_one_file = function
  | File_impl m -> Implem (read m)
  | File_intf m -> Interf (read m)
  | File_lex m -> Lex (read m)
  | File_yacc m -> Yacc (read m)
  | File_other f -> Other f
;;
(*i*)
}
(*i*)
