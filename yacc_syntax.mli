

(* locations for refering to CAML parts of yacc files *)

type location = Lex_syntax.location

type yacc_definitions =
    {
      header : location ;
      (* list of pairs (type,token names) *)
      token_decls : (location * string list) list;
      (* list of token properties, pairs (prop,token names) where prop is either  start, left, right or nonassoc) *)
      token_props : (string * string list) list;
      (* type of non-terminals *)
      type_decls : (location * string) list;
      (* grammar rules and actions *)
      rules : (string * (string list * location) list) list ;
      (* trailer *)
      trailer : location
    }




