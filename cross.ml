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

open Location
open Longident
open Output
open Printf
open Asttypes
open Parsetree

(* Cross references inside Caml files *)

type where = { w_filename : string; w_loc : int }
	       
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
      
module Stringset = Set.Make(struct type t = string let compare = compare end)
		     
let locals = ref Stringset.empty

module Qidset = Set.Make(struct type t = Longident.t let compare = compare end)

let opened = ref Qidset.empty

let current_file = ref ""

let current_location loc = 
  { w_filename = !current_file; w_loc = loc.loc_start }

let reset_cross () =
  opened := Qidset.empty;
  locals := Stringset.empty

let add_def loc s =
  if String.length s > 1 then begin
    add_global defined s (current_location loc)
  end

let add_open s =
  opened := Qidset.add s !opened

let add_local s =
  locals := Stringset.add s !locals
    
let add_uses loc s =
  if not (is_keyword s) 
    && String.length s > 1 && not (Stringset.mem s !locals) then
      add_global used s (current_location loc)
	
let add_uses_q loc q =
  let rec add = function
    | Lident s -> add_uses loc s
    | Ldot (q,s) -> add q; add_uses loc s
    | Lapply (q1,q2) -> add q1; add q2
  in
  add q

let iter_fst f = List.iter (fun x -> f (fst x))
    
let iter_snd f = List.iter (fun x -> f (snd x))
    
(* Traversing of Caml files *)

let option_iter f = function None -> ()  | Some x -> f x

let ids_of_a_pattern p =
  let r = ref [] in
  let add id = r := id :: !r in
  let rec pattern_d = function
    | Ppat_var id -> add id
    | Ppat_alias (p,id) -> add id; pattern p
    | Ppat_tuple pl -> List.iter pattern pl
    | Ppat_construct (_,po,_) -> option_iter pattern po
    | Ppat_record l -> iter_snd pattern l
    | Ppat_array pl -> List.iter pattern pl
    | Ppat_or (p1,p2) -> pattern p1; pattern p2
    | Ppat_constraint (p,_) -> pattern p
    | _ -> ()
  and pattern p = pattern_d p.ppat_desc
  in
  pattern p; !r

let pattern_for_def p =
  let loc = p.ppat_loc in
  let ids = ids_of_a_pattern p in
  List.iter (add_def loc) ids

let bind_variables ids f x =
  let save = !locals in
  List.iter add_local ids;
  f x;
  locals := save

let rec pattern_expression (p,e) =
  bind_variables (ids_of_a_pattern p) tr_expression e

and patterns_expression pl e =
  let ids = List.flatten (List.map ids_of_a_pattern pl) in
  bind_variables ids tr_expression e

and tr_expression e = 
  tr_expression_desc e.pexp_loc e.pexp_desc

and tr_expression_desc loc = function
  | Pexp_ident q -> add_uses_q loc q
  | Pexp_apply (e,el) ->
      tr_expression e; List.iter tr_expression el
  | Pexp_ifthenelse (e1,e2,e3) -> 
      tr_expression e1; tr_expression e2; option_iter tr_expression e3
  | Pexp_sequence (e1,e2) ->
      tr_expression e1; tr_expression e2
  | Pexp_while (e1,e2) ->
      tr_expression e1; tr_expression e2
  | Pexp_tuple el ->
      List.iter tr_expression el
  | Pexp_construct (_,e,_) -> 
      option_iter tr_expression e
  | Pexp_function pel -> List.iter pattern_expression pel
  | Pexp_match (e,pel) -> tr_expression e; List.iter pattern_expression pel
  | Pexp_try (e,pel) -> tr_expression e; List.iter pattern_expression pel
  | Pexp_let (recf,pel,e) -> 
      let pl = List.map fst pel in
      if recf = Recursive then 
	iter_snd (patterns_expression pl) pel
      else
	iter_snd tr_expression pel; 
      patterns_expression pl e
  | _ -> ()
(**
  | Pexp_constant _ -> ()
  | Pexp_record of (Longident.t * expression) list * expression option
  | Pexp_field of expression * Longident.t
  | Pexp_setfield of expression * Longident.t * expression
  | Pexp_array of expression list
  | Pexp_for of string * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type option * core_type option
  | Pexp_when of expression * expression
  | Pexp_send of expression * string
  | Pexp_new of Longident.t
  | Pexp_setinstvar of string * expression
  | Pexp_override of (string * expression) list
  | Pexp_letmodule of string * module_expr * expression
**)

and tr_value_description vd =
  () (* TODO *)

and tr_type_declaration td =
  () (* TODO *)

and tr_exception_declaration ed =
  () (* TODO *)

(* Classes *)

and tr_class_type_declaration ctd =
  () (* TODO *)

and tr_class_declaration cd =
  () (* TODO *)

(* Modules *)

and tr_module_type mt =
  () (* TODO *)

and tr_signature s =
  () (* TODO *)

and tr_module_expr me =
  () (* TODO *)

and tr_structure l = 
  List.iter tr_structure_item l

and tr_structure_item i =
  tr_structure_item_desc i.pstr_loc i.pstr_desc

and tr_structure_item_desc loc = function
  | Pstr_eval e -> 
      tr_expression e
  | Pstr_value (_,pel) -> 
      iter_fst pattern_for_def pel; iter_snd tr_expression pel
  | Pstr_primitive (id,vd) ->
      add_def loc id; tr_value_description vd
  | Pstr_type l ->
      iter_fst (add_def loc) l; iter_snd tr_type_declaration l
  | Pstr_exception (id,ed) ->
      add_def loc id; tr_exception_declaration ed
  | Pstr_module (id,me) ->
      add_def loc id; tr_module_expr me
  | Pstr_modtype (id,mt) ->
      add_def loc id; tr_module_type mt
  | Pstr_open m -> add_open m
  | Pstr_class l -> tr_class_declaration l
  | Pstr_class_type l -> tr_class_type_declaration l


let wrapper parsing_function traverse_function f =
  reset_cross ();
  current_file := f;
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  try
    traverse_function (parsing_function lexbuf);
    close_in c
  with Syntaxerr.Error _ | Syntaxerr.Escape_error | Lexer.Error _ -> begin
    eprintf " ** warning: syntax error while parsing %s\n" f;
    close_in c
  end

let cross_implem = wrapper Parse.implementation tr_structure

let cross_interf = wrapper Parse.interface tr_signature

