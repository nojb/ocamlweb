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
   and the modules opened *)

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

  let return_sets () =
    !opened, !defs

  let last = ref ""

}

let space = [' ' '\t']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let lo_ident = lowercase identchar*
let up_ident = uppercase identchar*
let ident = (lowercase | uppercase ) identchar*
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
           { exn_decl lexbuf; inside_code lexbuf }
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
           { exn_decl lexbuf; inside_code lexbuf }
  | "open" { opens lexbuf; inside_code lexbuf }
  | "let" | "and"
           { bindings lexbuf; inside_code lexbuf }
  | (up_ident '.')* lo_ident
           { add_uses (Lexing.lexeme lexbuf); inside_code lexbuf }
  | up_ident
           { inside_code lexbuf }
  | eof    { return_sets () }
  | _      { inside_code lexbuf }

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

and bindings = parse
  | space*   { bindings lexbuf }
  | lo_ident { add_local (Lexing.lexeme lexbuf); bindings lexbuf }
  | up_ident { bindings lexbuf }
  | eof      { () }
  | "="      { () }
  | '"'      { skip_string lexbuf; bindings lexbuf }
  | character{ bindings lexbuf }
  | _        { bindings lexbuf }

and opens = parse
  | up_ident { add_open (Lexing.lexeme lexbuf) }
  | space*   { opens lexbuf }
  | eof      { () }
  | _        { () }

(* Interface ***************************************************************)

and cross_interf = parse
  | space* { cross_interf lexbuf }
  | "val"  { val_decl lexbuf; cross_interf lexbuf }
  | "with" space+ "type"
           { cross_interf lexbuf }
  | "type" | "and"
           { type_decl lexbuf; cross_interf lexbuf }
  | "exception" 
           { exn_decl lexbuf; cross_interf lexbuf }
  | "open" { opens lexbuf; cross_interf lexbuf }
  | "(*"   { comment_depth := 1; skip_comment lexbuf; cross_interf lexbuf }
  | '"'    { skip_string lexbuf; cross_interf lexbuf }
  | ident  { add_uses (Lexing.lexeme lexbuf); cross_interf lexbuf }
  | eof    { return_sets () }
  | _      { cross_interf lexbuf }

(* val *)
and val_decl = parse
  | lo_ident      { add_def (Lexing.lexeme lexbuf) }
  | space*        { val_decl lexbuf }
  | eof           { () }
  | _             { () }

(* type *)
and type_decl = parse
  | '\'' lo_ident { type_decl lexbuf }
  | lo_ident      { add_def (Lexing.lexeme lexbuf) }
  | space*        { type_decl lexbuf }
  | eof           { () }
  | _             { () }

(* exception *)
and exn_decl = parse
  | up_ident      { add_def (Lexing.lexeme lexbuf) }
  | space*        { exn_decl lexbuf }
  | eof           { () }
  | _             { () }
  
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

