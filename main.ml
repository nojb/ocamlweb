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
open Filename
open Printf
open Output
open Web
open Doclexer
(*i*)


(*s \textbf{Usage.} Printed on error output. *)

let usage () =
  prerr_endline "";
  prerr_endline "Usage: ocamlweb <options and files>";
  prerr_endline "  -o <file>      write output in file <file>";
  prerr_endline "  -s             (short) no titles for files";
  prerr_endline "  --latex-sects  use LaTeX sectioning, not WEB";
  prerr_endline "  --header       do not skip the headers of Caml file";
  prerr_endline "  --no-preamble  suppress LaTeX header and trailer";
  prerr_endline "  --no-index     do not output the index";
  prerr_endline "  --extern-defs  keep external definitions in the index";
  prerr_endline "  --impl <file>  consider <file> as a .ml file";
  prerr_endline "  --intf <file>  consider <file> as a .mli file";
  prerr_endline "  --tex <file>   consider <file> as a .tex file";
  prerr_endline "  --latex-option <opt>";
  prerr_endline "                 pass an option to the LaTeX package ocamlweb.sty";
  prerr_endline "  --files <file> read file names to process in <file>";
  prerr_endline "  --quiet        quiet mode";
  prerr_endline "  --no-greek     disable use of greek letters for type variables";
  prerr_endline "";
  prerr_endline 
    "On-line documentation at http://www.lri.fr/~filliatr/ocamlweb/\n";
  exit 1


(*s \textbf{License informations.} Printed when using the option 
    \verb!--warranty!. *)

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


(*s \textbf{Banner.} Always printed. Notice that it is printed on error
    output, so that when the output of \ocamlweb\ is redirected this header
    is not (unless both standard and error outputs are redirected, of 
    course). *)

let banner () =
  eprintf "This is ocamlweb version %s, compiled on %s\n"
    Version.version Version.date;
  eprintf "Copyright (c) 1999 Jean-Christophe Filliâtre\n";
  eprintf "This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)\n";
  flush stderr
    

(*s \textbf{Separation of files.} Files given on the command line are
    separated according to their type, which is determined by their suffix.
    Implementations and interfaces have respective suffixes \verb!.ml!
    and \verb!.mli! and \LaTeX\ files have suffix \verb!.tex!. *)

let check_if_file_exists f =
  if not (Sys.file_exists f) then begin
    eprintf "\nocamlweb: %s: no such file\n" f;
    exit 1
  end

let what_file f =
  check_if_file_exists f;
  if check_suffix f ".ml" then
    File_impl (make_caml_file f)
  else if check_suffix f ".mli" then
    File_intf (make_caml_file f)
  else if check_suffix f ".tex" then
    File_other f
  else begin
    eprintf "\nocamlweb: don't know what to do with %s\n" f;
    exit 1
  end

(*s \textbf{Reading file names from a file.} 
    File names may be given
    in a file instead of being given on the command
    line. [(files_from_file f)] returns the list of file names contained
    in the file named [f]. These file names must be separated by spaces,
    tabulations or newlines.
 *)

let files_from_file f =
  let files_from_channel ch =
    let buf = Buffer.create 80 in
    let l = ref [] in
    try
      while true do
	match input_char ch with
	  | ' ' | '\t' | '\n' ->
	      if Buffer.length buf > 0 then l := (Buffer.contents buf) :: !l;
	      Buffer.clear buf
	  | c -> 
	      Buffer.add_char buf c
      done; []
    with End_of_file ->
      List.rev !l
  in
  try
    check_if_file_exists f;
    let ch = open_in f in
    let l = files_from_channel ch in
    close_in ch;l
  with Sys_error s -> begin
    eprintf "\nocamlweb: cannot read from file %s (%s)\n" f s;
    exit 1
  end

(*s \textbf{Parsing of the command line.} *)

let parse () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  let rec parse_rec = function
    | [] -> ()

    | ("-header" | "--header") :: rem ->
	skip_header := false; parse_rec rem
    | ("-latexsects" | "-latex-sects" | "--latex-sects") :: rem ->
	add_latex_option "latex-sects";
	web := false; parse_rec rem
    | ("-web" | "--web") :: rem ->
	web := true; parse_rec rem
    | ("-nopreamble" | "--nopreamble" | "--no-preamble") :: rem ->
	set_no_preamble true; parse_rec rem
    | ("-noindex" | "--noindex" | "--no-index") :: rem ->
	index := false; parse_rec rem
    | ("-o" | "--output") :: f :: rem ->
	set_output_to_file f; parse_rec rem
    | ("-o" | "--output") :: [] -> 
	usage ()
    | ("-s" | "--short") :: rem ->
	short := true; parse_rec rem
    | ("-extern-defs" | "--extern-defs") :: rem ->
	extern_defs := true; parse_rec rem
    | ("-q" | "-quiet" | "--quiet") :: rem ->
	quiet := true; parse_rec rem

    | ("--nogreek" | "--no-greek") :: rem ->
	use_greek_letters := false; parse_rec rem

    | ("-h" | "-help" | "-?" | "--help") :: rem ->
	banner (); usage ()
    | ("-v" | "-version" | "--version") :: _ ->
	banner (); exit 0
    | ("-warranty" | "--warranty") :: _ ->
	copying (); exit 0

    | "--latex-option" :: s :: rem ->
	add_latex_option s; parse_rec rem
    | "--latex-option" :: [] ->
	usage ()

    | ("-impl" | "--impl") :: f :: rem -> 
	check_if_file_exists f;
	let n = 
	  if Filename.check_suffix f ".mll" || Filename.check_suffix f ".mly"
          then Filename.chop_extension f else f
	in
	let m = File_impl { caml_filename = f; caml_module = module_name n } in
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
    | ("-files" | "--files") :: f :: rem ->
	List.iter (fun f -> add_file (what_file f)) (files_from_file f); 
	parse_rec rem
    | ("-files" | "--files") :: [] ->
	usage ()
    | f :: rem -> 
	add_file (what_file f); parse_rec rem
  in 
  parse_rec (List.tl (Array.to_list Sys.argv));
  List.rev !files


(*s \textbf{Main program.} Print the banner, parse the command line,
    read the files and then call [produce_document] from module [Web]. *)

let main () =
  let files = parse() in
  if not !quiet then banner();
  if List.length files > 0 then begin
    let l = List.map read_one_file files in
    produce_document l
  end

let _ = Printexc.catch main ()
