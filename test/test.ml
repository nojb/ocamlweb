(* header 1 *)
(* header 2 *)

(*i $Id$ i*)

(*i*)
open Hidden
(*i*)

(*p \newcommand{\mymacro}{Test macro in preamble} %my macro *)

(*s \mymacro. *)

(*s Constants. *)

let hexa = 0X4fff 
let hexa = 0x12BC 
let octal = 0O455 
let octal = 0o455 
let binary = 0B01010111001 
let binary = 0b00101011000
let float = 1.5152E-4
let float = 1.5152e-4

let a_string = "this is a constant string"

let a_longer_string = "this is a longer string\
    including newlines \
    and 4 leading spaces for the last two lines"

let an_array = [| [| 1; 3; 2 |]; [| 4; 5; 6 |] |]

(*s More types and exceptions declarations. *)

type stuff = AA | BB | CC

exception Exception of 'a foo * 'b bar * gee

type record = { 
  field1 : int;
  field2 : string 
}

(*s Test of symbols pretty-print. *)

let test_and x y = x && y

let test_or x y = x || y

let test_not x = not x

let test_not_equal x y = x <> y

let test_physical_equality x y = x == y

let test_physical_disequality x y = x != y

(*s A bit of documentation. I quote a function call [foo x] and I
    spend a few minutes speaking about [foo] and [x].  
    I can quote ["a string"] or a record value [{ x = 1; y = 2 }] 
    for instance. *) 

(*s Test of comments inside code. *)

let my_function x = function (y,z) ->
  x + y * z (* comment: $y\not=0$ *)

let test xor ory = xor + (*c comment *) ory

(*s Some documentation (no more considered as a comment due to the newline). *)

(*s Right-justified comments. *)

let autre_bout x =
  if x <= 0 then x+1 else x+2;          (*r a comment *)
  f 3;                                  (*r another *)
  cool ()                               (*r and a third one *)

(*s Patterns. *)

let test_pat = function
  | A1 -> 1
  | BB -> foo 2
  | CC -> C.f 3

(*s Test escaped dollar in comments: \$ *)
