%{

  open Lex_syntax
  open Yacc_syntax

  let dummy_loc =
    { start_pos = 0 ;
      end_pos = 0 ;
      start_line = 0 ;
      start_col = 0 }

%}

/*s yacc tokens */

%token Ttoken Tstart Ttype Tleft Tright Tnonassoc Tprec Terror
%token <Yacc_syntax.ident> Tident
%token <Yacc_syntax.location> Taction Ttypedecl
%token Tor Tsemicolon Tcolon Tmark
%token EOF

%start yacc_definitions 
%type <Yacc_syntax.yacc_definitions> yacc_definitions 

%%

/*s start symbol for yacc description files */

yacc_definitions: 
  header tokendecls Tmark rules header EOF 
  { { header = $1 ; 
      decls = $2;
      rules = $4;
      trailer = $5 } }
;

header :
  | Taction          
    { $1 }
  | /*epsilon*/      
    { dummy_loc }
;

tokendecls:
  | tokendecl tokendecls   
    { $1::$2 }
  | /*epsilon*/
    { [] }
;

tokendecl:
  | Ttoken Ttypedecl idlist
      { Typed_tokens($2,$3) }
  | Ttoken idlist
      { Untyped_tokens($2) }
  | Ttype Ttypedecl idlist
      { Non_terminals_type($2,$3) }
  | Tstart idlist
      { Start_symbols($2) }
  | Tleft idlist
      { Tokens_assoc($2) }
  | Tnonassoc idlist
      { Tokens_assoc($2) }
  | Tright idlist
      { Tokens_assoc($2) }
;

idlist:
  | Tident
    { [$1] }
  | Tident idlist
    { $1 :: $2 }
;

/*s entry for rules */
rules:
  | rule Tsemicolon rules    
    { $1 :: $3 }
  | /*epsilon*/
    { [] }
;

rule:
  | Tident Tcolon right_part 
    { ($1,$3) }
  | Tident Tcolon Tor right_part 
    { ($1,$4) }
;

right_part:
  | word Taction
    { [($1,$2)] }
  | word Taction Tor right_part
    { ($1,$2) :: $4 }
;

word:
  | /*epsilon*/
    { [] }
  | Tident word
    { $1 :: $2 }
  | Tprec Tident word
    { $2 :: $3 }
;


