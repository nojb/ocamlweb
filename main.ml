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

open Printf
open Output
open Web
open Doclexer

let usage () =
  prerr_endline "";
  prerr_endline "Usage: ocamlweb <options and files>";
  prerr_endline "  -o <file>      write output in file <file>";
  prerr_endline "  --no-web       no WEB style";
  prerr_endline "  --header       do not skip the headers of Caml file";
  prerr_endline "  --no-doc       suppress LaTeX header and trailer";
  prerr_endline "  --no-index     do not output the index";
  prerr_endline "  --extern-defs  keep external definitions in the index";
  prerr_endline "  --impl <file>  consider <file> as a .ml file";
  prerr_endline "  --intf <file>  consider <file> as a .mli file";
  prerr_endline "  --tex <file>   consider <file> as a .tex file";
  prerr_endline "  --latex-option <opt>";
  prerr_endline "                 pass an option to the LaTeX package ocamlweb.sty";
  exit 1

let copying () =
  prerr_endline "
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License version 2 for more details
(enclosed in the file GPL).";
  flush stderr

let banner () =
  eprintf "This is ocamlweb version %s, compiled on %s\n"
    Version.version Version.date;
  eprintf "Copyright (c) 1999 Jean-Christophe Filliâtre\n";
  eprintf "This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)\n";
  flush stderr

type caml_file = { caml_filename : string; caml_module : string }

type file_type =
    File_impl  of caml_file * caml_file option
  | File_intf  of caml_file
  | File_other of string

let module_name f = String.capitalize (Filename.basename f)

let make_impl f = 
  { caml_filename = f;
    caml_module = module_name (Filename.chop_suffix f ".ml") }

let make_intf f = 
  { caml_filename = f;
    caml_module = module_name (Filename.chop_suffix f ".mli") }

let check_if_file_exists f =
  if not (Sys.file_exists f) then begin
    eprintf "\nocamlweb: %s: no such file\n" f;
    exit 1
  end

let what_file f =
  check_if_file_exists f;
  if Filename.check_suffix f ".ml" then
    let fi = f^"i" in
    let intf = if Sys.file_exists fi then Some (make_intf fi) else None in
    File_impl (make_impl f, intf)
  else if Filename.check_suffix f ".mli" then
    File_intf (make_intf f)
   else if Filename.check_suffix f ".tex" then
    File_other f
   else begin
     eprintf "\nocamlweb: don't know what to do with %s\n" f;
    exit 1
  end

let parse () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  let rec parse_rec = function
      [] -> ()

    | ("-header" | "--header") :: rem ->
	skip_header := false; parse_rec rem
    | ("-noweb" | "--no-web") :: rem ->
	web := false; parse_rec rem
    | ("-web" | "--web") :: rem ->
	web := true; parse_rec rem
    | ("-nodoc" | "--nodoc" | "--no-doc") :: rem ->
	set_no_doc true; parse_rec rem
    | ("-noindex" | "--noindex" | "--no-index") :: rem ->
	index := false; parse_rec rem
    | ("-o" | "--output") :: f :: rem ->
	set_output_to_file f; parse_rec rem
    | ("-o" | "--output") :: [] -> 
	usage ()
    | ("-extern-defs" | "--extern-defs") :: rem ->
	extern_defs := true; parse_rec rem

    | ("-h" | "-help" | "-?" | "--help") :: rem ->
	usage ()
    | ("-v" | "-version" | "--version") :: _ ->
	exit 0
    | ("-warranty" | "--warranty") :: _ ->
	copying (); exit 0

    | "--latex-option" :: s :: rem ->
	add_latex_option s; parse_rec rem
    | "--latex-option" :: [] ->
	usage ()

    | ("-impl" | "--impl") :: f :: rem -> 
	check_if_file_exists f;
	let m = File_impl ({ caml_filename = f; caml_module = module_name f },
			   None) in
	add_file m; parse_rec rem
    | ("-impl" | "--impl") :: [] ->
	usage ()
    | ("-intf" | "--intf") :: f :: rem ->
	check_if_file_exists f;
	let i = File_intf { caml_filename = f; caml_module = module_name f } in
	add_file i; parse_rec rem
    | ("-intf" | "--intf") :: [] ->
	usage ()
    | ("-tex" | "--tex") :: f :: rem -> 
	add_file (File_other f); parse_rec rem
    | ("-tex" | "--tex") :: [] ->
	usage ()
    | f :: rem -> 
	add_file (what_file f); parse_rec rem
  in 
  parse_rec (List.tl (Array.to_list Sys.argv));
  List.rev !files

let raw_read_file f =
  reset_lexer ();
  let c = open_in f in
  let buf = Lexing.from_channel c in
  if !skip_header then header buf;
  let contents = implementation buf in
  close_in c;
  contents

let read_intf i = 
  { interf_file = i.caml_filename; 
    interf_name = i.caml_module; 
    interf_contents = raw_read_file i.caml_filename }
    
let read_impl (m,mi) =
  let interf = match mi with 
    | None -> None
    | Some i -> Some (read_intf i)
  in
  { implem_file = m.caml_filename; 
    implem_name = m.caml_module;
    implem_contents = raw_read_file m.caml_filename;
    implem_interf = interf }

let read_one_file = function
    File_impl (m,i) -> Implem (read_impl (m,i))
  | File_intf f -> Interf (read_intf f)
  | File_other f -> Other f

let main () =
  banner();
  let files = parse() in
  if List.length files > 0 then begin
    let modl = List.map read_one_file files in
    produce_document modl;
  end

let _ = Printexc.catch main ()
