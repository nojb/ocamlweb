

(* locations for refering to CAML parts of yacc files *)

type location = Lex_syntax.location

type token_decls =
    Typed_tokens of location * string list      (*r \verb|%token <type> ...| *)
  | Untyped_tokens of string list               (*r \verb|%token ...| *)
  | Typed_non_terminals of location * string list (*r \verb|%type <type> ...| *)
  | Non_terminals of string list          (*r \verb|%start ...| *)
  | Tokens_assoc of string list        (*r \verb|%left|, \verb|%right| or \verb|%nonassoc| *)


type yacc_definitions =
    {
      header : location ;
      token_decls : token_decls list;
      rules : (string * (string list * location) list) list ;
      trailer : location }




