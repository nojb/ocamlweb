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

  open Printf
  open Output

  let first_char lexbuf = Lexing.lexeme_char lexbuf 0

  let is_keyword = 
    let h = Hashtbl.create 101 in
    List.iter
      (fun key ->Hashtbl.add h  key ()) 
      [ "and"; "as";  "assert"; "begin"; "class";
	"constraint"; "do"; "done";  "downto"; "else"; "end"; "exception";
	"external";  "false"; "for";  "fun"; "function";  "functor"; "if";
	"in"; "include"; "inherit"; "initializer"; "lazy"; "let"; "match";
	"method";  "module";  "mutable";  "new"; "object";  "of";  "open";
	"or"; "parser";  "private"; "rec"; "sig";  "struct"; "then"; "to";
	"true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
	"mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"];
    function s -> try Hashtbl.find h s; true with Not_found -> false

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

  let first_line = ref true
		     
  let reset_pretty () =
    first_line := true

  let indentation n =
    if not !first_line then
      let kern = 1.5 *. (float n) in
      output_string (sprintf "\\ocwnl{%2.2fem}\n" kern)
    else
      first_line := false
	
}

let space = [' ' '\t']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

(* pretty-print of code, at beginning of line... *)
rule pr_code = parse
  | space* { let n = count_spaces (Lexing.lexeme lexbuf) in
	     indentation n;
	     pr_code_inside lexbuf;
	     pr_code lexbuf }
  | eof    { () }
   
(* ...and inside the line *)
and pr_code_inside = parse
  | '\n' { () }
  | lowercase identchar*
         { let s = Lexing.lexeme lexbuf in
	   if is_keyword s then output_keyword s else output_ident s;
	   pr_code_inside lexbuf }
  | _    { output_char (first_char lexbuf); pr_code_inside lexbuf }
  | eof  { () }
 
(* pretty-print of documentation (output `as it', except for quotations *)
and pr_doc = parse
  | _   { output_char (first_char lexbuf); pr_doc lexbuf }
  | eof { () }

{

  let pretty_print_code s =
    reset_pretty ();
    pr_code (Lexing.from_string s)

  let pretty_print_doc s =
    pr_doc (Lexing.from_string s)

}
