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
open Lexer

let usage () =
  prerr_endline "";
  prerr_endline "Usage: ocamlweb <options> <files>";
  prerr_endline "  -header   do not skip the header";
  prerr_endline "  -nodoc    suppress LaTeX header and trailer";
  prerr_endline "  -o file   write output in file `file'";
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

let parse () =
  let files = ref [] in
  let rec parse_rec = function
      [] -> ()

    | ("-header" | "--header") :: rem ->
	skip_header := false; parse_rec rem
    | ("-nodoc" | "--nodoc") :: rem ->
	set_no_doc true; parse_rec rem
    | ("-o" | "--output") :: f :: rem ->
	set_output_to_file f; parse_rec rem
    | ("-o" | "--output") :: [] -> 
	usage ()

    | ("-h" | "-help" | "-?" | "--help") :: rem ->
	usage ()
    | ("-v" | "-version" | "--version") :: _ ->
	exit 0
    | ("-warranty" | "--warranty") :: _ ->
	copying (); exit 0

    | s::rem -> files := s :: !files; parse_rec rem
  in 
  parse_rec (List.tl (Array.to_list Sys.argv));
  List.rev !files

let banner () =
  eprintf "This is ocamlweb version %s, compiled on %s\n"
    Version.version Version.date;
  eprintf "Copyright (c) 1999 Jean-Christophe Filliâtre\n";
  eprintf "This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)\n";
  flush stderr

let what_file f =
  let modulename f = String.capitalize (Filename.basename f) in
  if Filename.check_suffix f ".ml" then
    modulename (Filename.chop_suffix f ".ml"), ".ml"
  else if Filename.check_suffix f ".mli" then
    modulename (Filename.chop_suffix f ".mli"), ".mli"
  else
    f, ""

let check_if_file_exists f =
  if not (Sys.file_exists f) then begin
    eprintf "\nocamlweb: %s: no such file\n" f;
    exit 1
  end

let read_one_file f =
  check_if_file_exists f;
  let (mo,suff) = what_file f in
  if suff = ".ml" or suff = ".mli" then begin
    reset_lexer ();
    let c = open_in f in
    let buf = Lexing.from_channel c in
    if !skip_header then header buf;
    let file =
      if suff = ".ml" then 
	Implem { implem_name = mo; implem_contents = implementation buf }
      else 
	Interf { interf_name = mo; interf_contents = interface buf }
    in
    close_in c;
    file
  end else
    Other f

let main () =
  banner();
  let files = parse() in
  if List.length files > 0 then begin
    latex_header ();
    let modl = List.map read_one_file files in
    produce_document modl;
    latex_trailer ();
    close_output ()
  end

let _ = Printexc.catch main ()
