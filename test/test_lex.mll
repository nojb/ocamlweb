
(*i*){(*i*)

(*s Documentation header. *)

  let a = ref 0
  let b = ref 1

(*i*)}(*i*)

(*s Lexical rules. *)

rule token = parse
  | '!' { action }
