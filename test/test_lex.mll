
(*s Documentation header. *)

{

  let a = ref 0
  let b = ref 1

}

let ident = ['a'-'z']+

(*s Lexical rules. *)

rule token = parse
  | '!' { action }
  | ident as s { IDENT s }
  | eof { EOF }
