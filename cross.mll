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

  let defs = ref Stringset.empty
  let uses = ref Stringset.empty
  let opened = ref Stringset.empty
  let locals = ref Stringset.empty

  let reset_cross () =
    defs := Stringset.empty;
    uses := Stringset.empty;
    opened := Stringset.empty;
    locals := Stringset.empty

  let add_def s : unit =
    defs := Stringset.add s !defs

  let add_open s =
    opened := Stringset.add s !opened

  let add_local s =
    locals := Stringset.add s !locals

  let add_uses s =
    if not (is_keyword s) then
      uses := Stringset.add s !uses

  let code_return () =
    !opened, !defs, !uses

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

(* Code *********************************************************************)

rule cross_code = parse
  | space* { cross_code lexbuf }
  | "let"  { last := "let"; pattern_defs lexbuf; cross_code lexbuf }
  | "type" { last := "type"; type_decl lexbuf; cross_code lexbuf }
  | "and"  { if !last = "let" then pattern_defs lexbuf else type_decl lexbuf;
	     cross_code lexbuf }
  | "exception"
           { exn_decl lexbuf; cross_code lexbuf }
  | "open" { opens lexbuf; cross_code lexbuf }
  | "(*"   { comment_depth := 1; skip_comment lexbuf; cross_code lexbuf }
  | eof    { code_return () }
  | _      { cross_code lexbuf }

and pattern_defs = parse
  | space*   { pattern_defs lexbuf }
  | "rec"    { pattern_defs lexbuf }
  | lo_ident { add_def (Lexing.lexeme lexbuf); pattern_defs lexbuf }
  | eof      { () }
  | "="      { () }
  | _        { pattern_defs lexbuf }

and opens = parse
  | up_ident { add_open (Lexing.lexeme lexbuf) }
  | space*   { opens lexbuf }
  | eof      { () }
  | _        { () }

(* Interface ***************************************************************)

and cross_interf = parse
  | space* { cross_interf lexbuf }
  | "val"  { val_decl lexbuf; cross_interf lexbuf }
  | "type" | "and"
           { type_decl lexbuf; cross_interf lexbuf }
  | "exception" 
           { exn_decl lexbuf; cross_interf lexbuf }
  | "open" { opens lexbuf; cross_interf lexbuf }
  | "(*"   { comment_depth := 1; skip_comment lexbuf; cross_interf lexbuf }
  | ident  { add_uses (Lexing.lexeme lexbuf); cross_interf lexbuf }
  | eof    { code_return () }
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
  
(* Comments are ignored here ***********************************************)  
and skip_comment = parse
  | "(*" { incr comment_depth; skip_comment lexbuf }
  | "*)" { decr comment_depth;
           if !comment_depth > 0 then skip_comment lexbuf }
  | eof  { () }
  | _    { skip_comment lexbuf }
