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

(*i*)
open Filename
open Location
open Longident
open Output
open Printf
open Asttypes
open Parsetree
(*i*)

(*s Cross references inside Caml files are kept in the following two 
    global tables, which keep the places where things are defined and
    used, to produce the final indexes. *)

type where = { w_filename : string; w_loc : int }
	       
module Whereset = Set.Make(struct type t = where let compare = compare end)

type entry_type = 
  | Value
  | Constructor
  | Field
  | Label
  | Type
  | Exception
  | Module
  | ModuleType
  | Class
  | Method
  | LexParseRule       (*r CAMLLEX entry points *)
  | RegExpr            (*r CAMLLEX regular expressions *)
  | YaccNonTerminal    (*r CAMLYACC non-terminal symbols *)
  | YaccTerminal       (*r CAMLYACC terminal symbols, i.e. tokens *)
		    
type index_entry = { e_name : string; e_type : entry_type }

module Idmap = Map.Make(struct type t = index_entry let compare = compare end)
		 
let defined = ref Idmap.empty
let used = ref Idmap.empty


(*s The function [add_global] is a generic function to add an entry in one 
    table. [add_def] is used to add the definition of an identifier (so in the
    table [defined]). *)
	     
let add_global table k i =
  try
    let s = Idmap.find k !table in
    table := Idmap.add k (Whereset.add i s) !table
  with Not_found ->
    table := Idmap.add k (Whereset.singleton i) !table
      
let current_file = ref ""

let current_offset = ref 0

let current_location loc = 
  { w_filename = !current_file; w_loc = !current_offset + loc.loc_start }

let add_def loc t s =
  if String.length s > 1 then
    let e = { e_name = s; e_type = t } in
    add_global defined e (current_location loc)


(*s Another table, [locals], keeps the bound variables, in order to
    distinguish them from global identifiers. Then the function [add_uses]
    registers that an identifier is used (in the table [used]), taking care 
    of the fact that it is not a bound variable (in the table [locals]).
    [add_uses_q] iters [add_uses] on a qualified identifier. *)
      
module Stringset = Set.Make(struct type t = string let compare = compare end)
		     
let locals = ref Stringset.empty

let reset_cross f offs =
  assert (Stringset.cardinal !locals = 0);
  locals := Stringset.empty;
  current_file := f;
  current_offset := offs

let add_local s =
  locals := Stringset.add s !locals

let is_uppercase = function 'A'..'Z' -> true | _ -> false
    
let add_uses loc t s =
  if String.length s > 1 && 
     not (is_keyword s) && not (Stringset.mem s !locals) 
  then
    let e = { e_name = s; e_type = t } in
    add_global used e (current_location loc)
	
let add_uses_q loc t q =
  let rec addmod = function
    | Lident s -> add_uses loc Module s
    | Ldot (q,s) -> addmod q; add_uses loc Module s
    | Lapply (q1,q2) -> addmod q1; addmod q2 
  in
  match q with
    | Lident s -> add_uses loc t s
    | Ldot (q,s) -> addmod q; add_uses loc t s
    | Lapply (q1,q2) -> addmod q1; addmod q2

(*s Some useful functions. *)

let iter_fst f = List.iter (fun x -> f (fst x))
    
let iter_snd f = List.iter (fun x -> f (snd x))
    
let option_iter f = function None -> ()  | Some x -> f x


(*s When traversing a pattern, we must collect all its identifiers, in order
    to declare them as bound variables (or definitions behind a \textsf{let}
    construction). That is the job of the function [ids_of_a_pattern].
    Then [pattern_for_def] declares all the identifiers of a pattern as
    new definitions. *)

let ids_of_a_pattern p =
  let r = ref [] in
  let add id = r := id :: !r in
  let rec pattern_d = function
    | Ppat_any -> ()
    | Ppat_var id -> add id
    | Ppat_alias (p,id) -> add id; pattern p
    | Ppat_constant _ -> ()
    | Ppat_tuple pl -> List.iter pattern pl
    | Ppat_construct (_,po,_) -> option_iter pattern po
    | Ppat_record l -> iter_snd pattern l
    | Ppat_array pl -> List.iter pattern pl
    | Ppat_or (p1,p2) -> pattern p1; pattern p2
    | Ppat_constraint (p,_) -> pattern p
    | Ppat_variant (_,po) -> option_iter pattern po
    | Ppat_type _ -> ()
  and pattern p = 
    pattern_d p.ppat_desc
  in
  pattern p; !r

let pattern_for_def p =
  let loc = p.ppat_loc in
  let ids = ids_of_a_pattern p in
  List.iter (add_def loc Value) ids


(*s The following function locally adds some given variables to the set of
    bound variables, during the time of the application of a given function
    on a given argument. *)

let bind_variables ids f x =
  let save = !locals in
  List.iter add_local ids;
  f x;
  locals := save


(*s \textbf{Traversing of Caml abstract syntax trees.}
    Each type [t] in those abstract 
    syntax trees is associated to a function [tr_t] which traverses it,
    declaring the identifiers used and defined. Those types are defined
    in the Caml module interface [Paresetree.mli] contained in the Caml source
    distribution. 
 
    The following code is quite code, but systematic and easy to understand.
 *)

(*s Core types. *)

let rec tr_core_type t =
  tr_core_type_desc t.ptyp_loc t.ptyp_desc

and tr_core_type_desc loc = function
  | Ptyp_any | Ptyp_var _ -> 
      ()
  | Ptyp_arrow (l,t1,t2) ->
      add_def loc Label l; tr_core_type t1; tr_core_type t2
  | Ptyp_tuple tl ->
      List.iter tr_core_type tl
  | Ptyp_constr (q,tl) ->
      add_uses_q loc Type q; List.iter tr_core_type tl
  | Ptyp_object l ->
      List.iter tr_core_field_type l
  | Ptyp_class (id,l,ll) ->
      add_uses_q loc Class id;
      List.iter (add_def loc Label) ll;
      List.iter tr_core_type l
  | Ptyp_alias (ct,_) -> 
      tr_core_type ct
  | Ptyp_variant (l,_,_) ->
      List.iter (fun (_,_,ctl) -> List.iter tr_core_type ctl) l

and tr_core_field_type ft =
  tr_core_field_desc ft.pfield_loc ft.pfield_desc

and tr_core_field_desc loc = function
  | Pfield (id,ct) ->
      add_uses loc Method id;
      tr_core_type ct
  | Pfield_var -> ()

(*s Type expressions for the class language. *)

let tr_class_infos f p =
  add_def p.pci_loc Class p.pci_name;
  f p.pci_expr

(*s Value expressions for the core language. *)

let bind_pattern f (p,e) =
  bind_variables (ids_of_a_pattern p) f e

let bind_patterns f pl e =
  let ids = List.flatten (List.map ids_of_a_pattern pl) in
  bind_variables ids f e


let rec tr_expression e = 
  tr_expression_desc e.pexp_loc e.pexp_desc

and tr_expression_desc loc = function
  | Pexp_ident q -> 
      add_uses_q loc Value q
  | Pexp_apply (e,lel) ->
      tr_expression e; 
      List.iter (fun (l,e) -> add_uses loc Label l; tr_expression e) lel
  | Pexp_ifthenelse (e1,e2,e3) -> 
      tr_expression e1; tr_expression e2; option_iter tr_expression e3
  | Pexp_sequence (e1,e2) ->
      tr_expression e1; tr_expression e2
  | Pexp_while (e1,e2) ->
      tr_expression e1; tr_expression e2
  | Pexp_tuple el ->
      List.iter tr_expression el
  | Pexp_construct (q,e,_) -> 
      add_uses_q loc Constructor q;
      option_iter tr_expression e
  | Pexp_function (l,eo,pel) -> 
      add_def loc Label l;
      option_iter tr_expression eo;
      List.iter (bind_pattern tr_expression) pel
  | Pexp_match (e,pel) -> 
      tr_expression e; List.iter (bind_pattern tr_expression) pel
  | Pexp_try (e,pel) -> 
      tr_expression e; List.iter (bind_pattern tr_expression) pel
  | Pexp_let (recf,pel,e) -> 
      let pl = List.map fst pel in
      if recf = Recursive then 
	iter_snd (bind_patterns tr_expression pl) pel
      else
	iter_snd tr_expression pel; 
      bind_patterns tr_expression pl e
  | Pexp_record (l,e) ->
      iter_fst (add_uses_q loc Field) l; iter_snd tr_expression l; 
      option_iter tr_expression e
  | Pexp_field (e,q) ->
      tr_expression e; add_uses_q loc Field q
  | Pexp_setfield (e1,q,e2) ->
      tr_expression e1; add_uses_q loc Field q; tr_expression e2
  | Pexp_array el ->
      List.iter tr_expression el
  | Pexp_for (i,e1,e2,_,e) ->
      tr_expression e1; tr_expression e2; bind_variables [i] tr_expression e
  | Pexp_constraint (e,t1,t2) ->
      tr_expression e; option_iter tr_core_type t1; option_iter tr_core_type t2
  | Pexp_when (e1,e2) ->
      tr_expression e1; tr_expression e2
  | Pexp_letmodule (x,m,e) ->
      tr_module_expr m; bind_variables [x] tr_expression e
  | Pexp_constant _ -> ()
  | Pexp_send (e,id) ->
      add_uses loc Method id; tr_expression e
  | Pexp_new id ->
      add_uses_q loc Class id
  | Pexp_setinstvar (id,e) ->
      add_uses loc Value id; tr_expression e
  | Pexp_override l ->
      iter_fst (add_uses loc Method) l; iter_snd tr_expression l
  | Pexp_variant (_,eo) ->
      option_iter tr_expression eo

(*s Value descriptions. *)

and tr_value_description vd =
  tr_core_type vd.pval_type

(*s Type declarations. *)

and tr_type_declaration td =
  tr_type_kind td.ptype_loc td.ptype_kind

and tr_type_kind loc = function
  | Ptype_abstract -> ()
  | Ptype_variant cl ->
      iter_fst (add_def loc Constructor) cl;
      iter_snd (List.iter tr_core_type) cl
  | Ptype_record fl ->
      List.iter (fun (f,_,t) -> add_def loc Field f; tr_core_type t) fl

and tr_exception_declaration ed =
  List.iter tr_core_type ed

(*s Type expressions for the class language. *)

and tr_class_type c =
  tr_class_type_desc c.pcty_loc c.pcty_desc

and tr_class_type_desc loc = function
  | Pcty_constr (id,l) ->
      add_uses_q loc Class id;
      List.iter tr_core_type l
  | Pcty_signature cs ->
      tr_class_signature cs
  | Pcty_fun (l,co,cl) ->
      add_def loc Label l;
      tr_core_type co;
      tr_class_type cl

and tr_class_signature (ct,l) = 
  tr_core_type ct;
  List.iter tr_class_type_field l

and tr_class_type_field = function
  | Pctf_inher ct -> 
      tr_class_type ct
  | Pctf_val (id,_,ct,loc) ->
      add_def loc Value id;
      option_iter tr_core_type ct
  | Pctf_virt (id,_,ct,loc) ->
      add_def loc Method id;
      tr_core_type ct
  | Pctf_meth (id,_,ct,loc) ->
      add_def loc Method id;
      tr_core_type ct
  | Pctf_cstr (ct1,ct2,_) ->
      tr_core_type ct1;
      tr_core_type ct2

and tr_class_description x = tr_class_infos tr_class_type x

and tr_class_type_declaration x = tr_class_infos tr_class_type x 

(*s Value expressions for the class language. *)

and tr_class_expr ce = tr_class_expr_desc ce.pcl_loc ce.pcl_desc

and tr_class_expr_desc loc = function
  | Pcl_constr (id,l) ->
      add_uses_q loc Class id;
      List.iter tr_core_type l
  | Pcl_structure cs -> 
      tr_class_structure cs
  | Pcl_fun (l,eo,p,ce) ->
      add_def loc Label l;
      option_iter tr_expression eo;
      bind_variables (ids_of_a_pattern p) tr_class_expr ce
  | Pcl_apply (ce,l) ->
      tr_class_expr ce;
      List.iter (fun (l,e) -> add_uses loc Label l; tr_expression e) l
  | Pcl_let (recf,pel,ce) -> 
      let pl = List.map fst pel in
      if recf = Recursive then 
	iter_snd (bind_patterns tr_expression pl) pel
      else
	iter_snd tr_expression pel; 
      bind_patterns tr_class_expr pl ce
  | Pcl_constraint (ce,ct) ->
      tr_class_expr ce;
      tr_class_type ct

and tr_class_structure (p,l) = 
  List.iter (fun f -> bind_pattern tr_class_field (p,f)) l

and tr_class_field = function
  | Pcf_inher (ce,_) ->
      tr_class_expr ce
  | Pcf_val (id,_,e,loc) ->
      add_def loc Value id;
      tr_expression e
  | Pcf_virt(id,_,ct,loc) ->
      add_def loc Method id;
      tr_core_type ct
  | Pcf_meth (id,_,e,loc) ->
      add_def loc Method id;
      tr_expression e
  | Pcf_cstr (ct1,ct2,_) ->
      tr_core_type ct1;
      tr_core_type ct2
  | Pcf_let (recf,pel,_) -> 
      let pl = List.map fst pel in
      if recf = Recursive then 
	iter_snd (bind_patterns tr_expression pl) pel
      else
	iter_snd tr_expression pel
  | Pcf_init e ->
      tr_expression e

and tr_class_declaration x = tr_class_infos tr_class_expr x

(*s Type expressions for the module language. *)

and tr_module_type mt =
  tr_module_type_desc mt.pmty_loc mt.pmty_desc

and tr_module_type_desc loc = function
  | Pmty_ident id -> 
      add_uses_q loc ModuleType id
  | Pmty_signature s -> 
      tr_signature s
  | Pmty_functor (id,mt1,mt2) -> 
      tr_module_type mt1;
      bind_variables [id] tr_module_type mt2
  | Pmty_with (mt,cl) ->
      tr_module_type mt;
      List.iter 
	(fun (id,c) -> add_uses_q loc Type id; tr_with_constraint loc c) cl

and tr_signature s =
  List.iter tr_signature_item s

and tr_signature_item i =
  tr_signature_item_desc i.psig_loc i.psig_desc

and tr_signature_item_desc loc = function
  | Psig_value (x,vd) ->
      add_def loc Value x; tr_value_description vd
  | Psig_type l ->
      iter_fst (add_def loc Type) l; iter_snd tr_type_declaration l
  | Psig_exception (id,ed) ->
      add_def loc Exception id; tr_exception_declaration ed
  | Psig_module (id,mt) ->
      add_def loc Module id; tr_module_type mt
  | Psig_modtype (id,mtd) ->
      add_def loc ModuleType id; tr_modtype_declaration mtd
  | Psig_open q -> 
      add_uses_q loc Module q
  | Psig_include mt ->
      tr_module_type mt
  | Psig_class l ->
      List.iter tr_class_description l
  | Psig_class_type l ->
      List.iter tr_class_type_declaration l

and tr_modtype_declaration = function
  | Pmodtype_abstract -> ()
  | Pmodtype_manifest mt -> tr_module_type mt

and tr_with_constraint loc = function
  | Pwith_type td -> tr_type_declaration td
  | Pwith_module id -> add_uses_q loc Module id

(*s Value expressions for the module language. *)

and tr_module_expr me =
  tr_module_expr_desc me.pmod_loc me.pmod_desc

and tr_module_expr_desc loc = function
  | Pmod_ident id -> 
      add_uses_q loc Module id
  | Pmod_structure s -> 
      tr_structure s
  | Pmod_functor (id,mt,me) ->
      tr_module_type mt;
      bind_variables [id] tr_module_expr me
  | Pmod_apply (me1,me2) ->
      tr_module_expr me1;
      tr_module_expr me2
  | Pmod_constraint (me,mt) ->
      tr_module_expr me;
      tr_module_type mt

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
      add_def loc Value id; tr_value_description vd
  | Pstr_type l ->
      iter_fst (add_def loc Type) l; iter_snd tr_type_declaration l
  | Pstr_exception (id,ed) ->
      add_def loc Exception id; tr_exception_declaration ed
  | Pstr_module (id,me) ->
      add_def loc Module id; tr_module_expr me
  | Pstr_modtype (id,mt) ->
      add_def loc ModuleType id; tr_module_type mt
  | Pstr_open m -> 
      add_uses_q loc Module m
  | Pstr_class l -> 
      List.iter tr_class_declaration l
  | Pstr_class_type l -> 
      List.iter tr_class_type_declaration l
  | Pstr_exn_rebind (id,q) ->
      add_def loc Exception id;
      add_uses_q loc Exception q

(*s Given all that collecting functions, we can now define two functions
    [cross_implem] and [cross_interf] which respectively compute the 
    cross-references in implementations and interfaces. *)

let add_module m = 
  add_def { loc_start = 0; loc_end = 0; loc_ghost = false } Module m

let wrapper parsing_function traverse_function f m =
  reset_cross f 0;
  add_module m;
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  try
    traverse_function (parsing_function lexbuf);
    close_in c
  with Syntaxerr.Error _ | Syntaxerr.Escape_error | Lexer.Error _ -> begin
    if not !quiet then
      eprintf " ** warning: syntax error while parsing %s\n" f;
    close_in c
  end

let cross_implem = wrapper Parse.implementation tr_structure

let cross_interf = wrapper Parse.interface tr_signature

(*s cross-referencing lex and yacc description files *)

let lexer_function_inside_file ic loc =
  seek_in ic loc.Lex_syntax.start_pos;
  let left = ref (loc.Lex_syntax.end_pos - loc.Lex_syntax.start_pos) in
  fun buf len ->
    let m = input ic buf 0 (min !left len) in
    for i=0 to pred m do
      if String.get buf i = '$' then String.set buf i ' '
    done;
    left := !left - m;
    m

let cross_action_inside_file f m loc = 
  reset_cross f loc.Lex_syntax.start_pos;
  let c = open_in f in
  let lexbuf = Lexing.from_function (lexer_function_inside_file c loc) in
  try
    tr_structure (Parse.implementation lexbuf);
    close_in c
  with Syntaxerr.Error _ | Syntaxerr.Escape_error | Lexer.Error _ -> begin
    if not !quiet then
      eprintf " ** warning: syntax error while parsing action in %s\n" f;
    close_in c
  end

(*s cross-referencing lex description files *)

let add_used_regexps f m r = ()

let traverse_lex_defs f m lexdefs =
  (* traverse header *)
  cross_action_inside_file f m lexdefs.Lex_syntax.header;
  (* traverse named regexps *)
  List.iter
    (fun (id,regexp) -> 
       (*i add_def id RegExpr; i*)
       add_used_regexps f m regexp)
    lexdefs.Lex_syntax.named_regexps;
  (* traverse lexer rules *)
  List.iter
    (fun (id,rules) -> 
       (*i add_defs id LexParseRule; i*)
       List.iter 
	 (fun (regexp,action) ->
	    add_used_regexps f m regexp;
	    cross_action_inside_file f m action)
	 rules)
    lexdefs.Lex_syntax.entrypoints;
  (* traverse trailer *)
  cross_action_inside_file f m lexdefs.Lex_syntax.trailer

  

let cross_lex f m =
  reset_cross f 0;
  add_module m;
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  try
    let lexdefs = Lex_parser.lexer_definition Lex_lexer.main lexbuf in
    traverse_lex_defs f m lexdefs;
    close_in c
  with Parsing.Parse_error | Lex_lexer.Lexical_error _ -> begin
    if not !quiet then
      eprintf " ** warning: syntax error while parsing %s\n" f;
    close_in c
  end
  
(*s cross-referencing yacc description files *)

let traverse_yacc f m yacc_defs = 
  (* traverse header *)
  cross_action_inside_file f m yacc_defs.Yacc_syntax.header;
  (* traverse token decls *)
  (*i add_defs Terminal i*)
  (* traverse grammar rules *)
  List.iter
    (fun (lhs,rhs) ->
       (*i add_def lhs YaccNonTerminal; i*)
       List.iter
	 (fun (rhs,action) ->
	    (*i add_used rhs i*)
	    cross_action_inside_file f m action)
	 rhs)
    yacc_defs.Yacc_syntax.rules;
  (* traverse trailer *)
  cross_action_inside_file f m yacc_defs.Yacc_syntax.trailer

let cross_yacc f m =
  reset_cross f 0;
  add_module m;
  let c = open_in f in
  let lexbuf = Lexing.from_channel c in
  try
    let yacc_defs = Yacc_parser.yacc_definitions Yacc_lexer.main lexbuf in
    traverse_yacc f m yacc_defs;
    close_in c
  with Parsing.Parse_error -> begin
    if not !quiet then
      eprintf " ** warning: syntax error while parsing %s\n" f;
    close_in c
  end
    | Yacc_lexer.Lexical_error(msg,line,col) -> begin
    if not !quiet then
      eprintf " ** warning: while parsing %s, lexical error (%s) at line %d, character %d\n" f msg line col;
    close_in c
  end



