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

type paragraph =
    Documentation of string
  | Code of string

type section =
    { section_ids : string list;
      section_contents : paragraph list }

type implem = 
    { implem_name : string;
      implem_contents : section list }

type decl = { 
  decl_id : string;
  decl_contents : string;
  decl_spec : string }

type interf = { 
  interf_name : string;
  interf_contents : decl list }

type file = 
    Implem of implem
  | Interf of interf
  | Other  of string

