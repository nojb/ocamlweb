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

let usage () =
  prerr_endline "";
  prerr_endline "Usage: ocamlweb <options> <files>";
  exit 1

let parse () =
  let rec parse_rec = function
      [] -> ()
    | _ -> usage()
  in 
  parse_rec (List.tl (Array.to_list Sys.argv))

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

let main () =
  banner()

let _ = Printexc.catch main ()
