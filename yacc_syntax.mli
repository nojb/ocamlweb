

(* locations for refering to CAML parts of yacc files *)

type location = Lex_syntax.location

type ident = string * location

type token_decls =
  | Typed_tokens of location * ident list      (*r \verb|%token <type> ...| *)
  | Untyped_tokens of ident list               (*r \verb|%token ...| *)
  | Non_terminals_type of location * ident list (*r \verb|%type <type> ...| *)
  | Start_symbols of ident list          (*r \verb|%start ...| *)
  | Tokens_assoc of ident list        (*r \verb|%left|, \verb|%right| or \verb|%nonassoc| *)


type yacc_definitions =
    {
      header : location ;
      decls : token_decls list;
      rules : (ident * (ident list * location) list) list ;
      trailer : location
    }





