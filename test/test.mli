(* HEADER THAT SHOULD NOT APPEAR! *)

(* \tableofcontents *)

(*i $Id$ i*)

(*s Types declarations. *)

type t = int

type u = int * int

type v = int -> int

type 'a my_type = 'a -> 'a

type 'a my_type' =
  | A
  | B of 'a
  | C of (foo -> bar)
  | D of foo * (bar -> bar)

type new_type = my_type

(*s Function prototypes. *)

val foo : 'a -> 'b -> 'c

val map : ('a -> 'b) -> 'a list -> 'b list

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
