(* HEADER QUI DEVRAIT ÊTRE IGNORÉ !! *)

(*i $Id$ i*)

(*s Déclarations de types. *)

type t = int

type t = int * int

type t = int -> int

type 'a t = 'a -> 'a

type 'a t =
  | A
  | B of 'a
  | C of foo -> bar
  | D of foo * (bar -> bar)


