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

(* This module parses code to extract the identifiers defined and used,
   and the modules opened. *)

{

  open Output

  let comment_depth = ref 0

  module Stringset = Set.Make(struct type t = string let compare = compare end)

  type where = { w_module : string; w_section : int }

  module Whereset = Set.Make(struct type t = where let compare = compare end)

  module Idmap = Map.Make(struct type t = string let compare = compare end)

  (* Those two global tables keep the places where things are defined and
     used, to produce the final indexes *)

  let defined = ref Idmap.empty
  let used = ref Idmap.empty

  let add_global table k i =
    try
      let s = Idmap.find k !table in
      table := Idmap.add k (Whereset.add i s) !table
    with Not_found ->
      table := Idmap.add k (Whereset.add i Whereset.empty) !table

  (* The following tables are used locally, at each step *)

  let opened = ref Stringset.empty
  let defs = ref Stringset.empty
  let locals = ref Stringset.empty

  let current_module = ref ""
  let cross_new_module m = current_module := m

  let current_section = ref 0
  let cross_new_section s = current_section := s

  let current_location () = 
    { w_module = !current_module; w_section = !current_section }

  let reset_cross () =
    opened := Stringset.empty;
    locals := Stringset.empty

  let add_def s =
    if String.length s > 1 then begin
      add_global defined s (current_location());
      defs := Stringset.add s !defs
    end

  let add_open s =
    opened := Stringset.add s !opened

  let add_local s =
    locals := Stringset.add s !locals

  let add_uses s =
    if not (is_keyword s) 
       && String.length s > 1 && not (Stringset.mem s !locals) then
      add_global used s (current_location())

  let add_uses_q s =
    let n = String.length s in
    let rec decomp i =
      try
	let k = String.index_from s i '.' in
	(String.sub s i (k-i)) :: (decomp (succ k))
      with Not_found -> 
	[String.sub s i (n-i)]
    in
    List.iter add_uses (decomp 0)

  let return_sets () =
    !opened, !defs

  let last = ref ""

}

let space = [' ' '\t']
let space_or_nl = [' ' '\t' '\n']

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let lo_ident = lowercase identchar*
let up_ident = uppercase identchar*
let ident = (lowercase | uppercase ) identchar*

let lo_qident = (up_ident '.')* lo_ident
let up_qident = (up_ident '.')* up_ident
let qident = (up_ident '.')* ident

let character = 
  "'" ( [^ '\\' '\''] | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] 
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] ) "'"

(* Code *********************************************************************)

rule cross_code = parse
  | space* { cross_code lexbuf }
  | "let" space*
           { last := "let"; pattern_defs lexbuf; inside_code lexbuf }
  | "type" { last := "type"; type_decl lexbuf; inside_code lexbuf }
  | "and"  { if !last = "let" then pattern_defs lexbuf else type_decl lexbuf;
	     inside_code lexbuf }
  | "exception"
           { def_ident lexbuf; inside_code lexbuf }
  | "open" { opens lexbuf; cross_code lexbuf }
  | "(*"   { comment_depth := 1; skip_comment lexbuf; cross_code lexbuf }
  | character
           { cross_code lexbuf }
  | '"'    { skip_string lexbuf; cross_code lexbuf }
  | eof    { return_sets () }
  | _      { cross_code lexbuf }

and inside_code = parse
  | "(*"   { comment_depth := 1; skip_comment lexbuf; inside_code lexbuf }
  | '"'    { skip_string lexbuf; inside_code lexbuf }
  | character
           { inside_code lexbuf }
  | "exception"
           { def_ident lexbuf; inside_code lexbuf }
  | "open" { opens lexbuf; inside_code lexbuf }
  | "let" | "and" | "function" | "fun"
           { bindings lexbuf; inside_code lexbuf }
  | qident
           { add_uses_q (Lexing.lexeme lexbuf); inside_code lexbuf }
  | up_ident
           { inside_code lexbuf }
  | eof    { return_sets () }
  | _      { inside_code lexbuf }

(* stops on a character '=' *)
and pattern_defs = parse
  | space* "rec" 
             { pattern_defs lexbuf }
  | lo_ident { add_def (Lexing.lexeme lexbuf); pattern_defs lexbuf }
  | up_ident { pattern_defs lexbuf }
  | eof      { () }
  | '='      { () }
  | '"'      { skip_string lexbuf; pattern_defs lexbuf }
  | character{ pattern_defs lexbuf }
  | space    { bindings lexbuf }
  | _        { pattern_defs lexbuf }

(* stops on '=' or '->' *)
and bindings = parse
  | space*   { bindings lexbuf }
  | lo_ident { add_local (Lexing.lexeme lexbuf); bindings lexbuf }
  | up_ident { bindings lexbuf }
  | eof      { () }
  | "=" | "->" { () }
  | '"'      { skip_string lexbuf; bindings lexbuf }
  | character{ bindings lexbuf }
  | _        { bindings lexbuf }

and opens = parse
  | (up_ident '.')* up_ident
             { add_open (Lexing.lexeme lexbuf) }
  | space*   { opens lexbuf }
  | eof      { () }
  | _        { () }

(* Interface ***************************************************************)

and cross_interf = parse
  | space* { cross_interf lexbuf }
  | "val" | "external" | "exception" 
           { def_ident lexbuf; cross_interf lexbuf }
  | "with" space+ "type"
           { cross_interf lexbuf }
  | "type" | "and"
           { type_decl lexbuf; cross_interf lexbuf }
  | "module" | "module" space* "type"
           { def_ident lexbuf; cross_interf lexbuf }
  | "open" { opens lexbuf; cross_interf lexbuf }
  | "(*"   { comment_depth := 1; skip_comment lexbuf; cross_interf lexbuf }
  | '"'    { skip_string lexbuf; cross_interf lexbuf }
  | "'" lo_ident
           { cross_interf lexbuf }
  | qident { add_uses_q (Lexing.lexeme lexbuf); cross_interf lexbuf }
  | eof    { return_sets () }
  | _      { cross_interf lexbuf }

(* ident defined *)
and def_ident = parse
  | ident         { add_def (Lexing.lexeme lexbuf) }
  | space_or_nl*  { def_ident lexbuf }
  | eof           { () }
  | _             { () }

(* type definition *)
and type_decl = parse
  | '\'' lo_ident { type_decl lexbuf }
  | lo_ident      { add_def (Lexing.lexeme lexbuf); type_def_body lexbuf }
  | '(' | ',' | ')' | space*        
                  { type_decl lexbuf }
  | eof           { () }
  | _             { () }

and type_def_body = parse
  | up_ident      { add_def (Lexing.lexeme lexbuf); type_def_body lexbuf }
  | qident        { add_uses_q (Lexing.lexeme lexbuf); type_def_body lexbuf }
  | "\n\n"        { () }
  | space_or_nl*  { type_def_body lexbuf }
  | '{'           { record_label lexbuf; type_def_body lexbuf }
  | eof           { () }
  | _             { type_def_body lexbuf }

(* records *)
and record_label = parse
  | lo_ident      { add_def (Lexing.lexeme lexbuf); record_body lexbuf }
  | space_or_nl*  { record_label lexbuf }
  | eof           { () }
  | _             { () }

and record_body = parse
  | qident { add_uses_q (Lexing.lexeme lexbuf); record_body lexbuf }
  | ';'    { record_label lexbuf; record_body lexbuf }
  | '}'    { () }
  | eof    { () }
  | _      { record_body lexbuf }

(* Comments and strings are ignored here ***********************************)  
and skip_comment = parse
  | "(*" { incr comment_depth; skip_comment lexbuf }
  | "*)" { decr comment_depth;
           if !comment_depth > 0 then skip_comment lexbuf }
  | eof  { () }
  | _    { skip_comment lexbuf }

and skip_string = parse
  | '"'      { () }
  | '\\' '"' { skip_string lexbuf }
  | eof      { () }
  | _        { skip_string lexbuf }

