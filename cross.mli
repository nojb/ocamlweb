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

(*s That module exports to global tables [used] and [defined], indexed
   by identifiers (strings) and containing respectively the sets of locations
   where they are defined and used.
   Those locations are of type [where], which contain the name of the file
   and the absolute position in the source.
 *)
   
type where = { w_filename : string; w_loc : int }

type entry_type = 
  | Value
  | Constructor
  | Field
  | Type
  | Exception
  | Module
  | ModuleType
  | Class
  | Method
		    
type index_entry = { e_name : string; e_type : entry_type }

module Idmap : Map.S with type key = index_entry

module Stringset : Set.S with type elt = string

module Whereset : Set.S with type elt = where

val used : Whereset.t Idmap.t ref
val defined : Whereset.t Idmap.t ref

(*s The two following functions fill the above table for a given file. *)

val cross_implem : string -> string -> unit
val cross_interf : string -> string -> unit
