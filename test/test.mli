(* HEADER QUI DEVRAIT ÊTRE IGNORÉ !! *)

(*i $Id$ i*)

(*s Déclarations de types. *)

type t = int

type u = int * int

type v = int -> int

type 'a mon_type = 'a -> 'a

type 'a mon_type' =
  | A
  | B of 'a
  | C of (foo -> bar)
  | D of foo * (bar -> bar)


