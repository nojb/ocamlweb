/*
 Gros blabla

!!!!!!PROBLEME SUR DECL_LIST : utilisé en 9 et défini en 10 !!!!!!!!!!!!!

*/
/* id ..*/

%{
  open Exceptions;;
  open Asa_expr;;


(*s parse\_error s.
Redefinition de la fonction d'erreur de parsing.
Transmet au module principal plus de details sur l'erreur, en particulier :
1- le numero de ligne de l'erreur,
2- le caractère dans la ligne où est l'erreur.  *)


  let parse_error s =
    let error = Parser_error(
				Parsing.symbol_start(),
				Parsing.symbol_end(),
				"" ) 
    in raise error
  ;;
  let warning = ref false;;

%}

/*s Definition des tokens de l'analyse lexicale. */
%token EOF LIFULA
%token PLUS MOINS MULT DIV
%token PARENT_G PARENT_D
%token <string> IDENT
%token <int> CSTE
%token <string> CHAINE 
%token TRUE FALSE
%token LET NOT IN REC IF THEN ELSE
%token INT BOOL STRING
%token VIRGULE DEUX_POINTS
%token NOT ET OU
%token EXP   
%token EGAL DIFF
%token SUP SUPEGAL
%token INF INFEGAL

/*s Definition des precedences. */
%right LET
%left IN

%right IF           /*r precedence minimale. */
%left THEN
%left ELSE

%left EGAL DIFF
%left SUP SUPEGAL
%left INF INFEGAL

%left OU
%left ET

%left PLUS MOINS      
%left MULT DIV
%nonassoc UMINUS
%right EXP          /*c precedence maximale. */

/*s Definition du pt entree */
%start main

/*s Definition du type */
%type <Asa_expr.asa_expr> main

%%

/*s Definition MAIN */
main:
  LIFULA expr EOF  /* Gestion du token LIFULA prevenant l'incoherence  eventuelle */
      { $2 }      /* d'erreurs si l'on appelle lifulac sur un fichier binaire par ex. */
| expr EOF               
      { 
    	warning:=true;
	$1
      }
;


/*s Definition EXPR */  
expr:
|  PARENT_G expr PARENT_D
    { $2 } 
| LET def IN expr 
    { Let($2,$4)  }
| expr PLUS  expr  
    { Plus($1,$3) }
| expr MOINS expr 
    { Moins($1,$3) }
| expr  MULT  expr 
    { Mult($1,$3) }
| expr DIV   expr 
    { Div($1,$3) }
| expr EXP   expr 
    { Exp($1,$3) }
| MOINS expr   %prec UMINUS         
   { Opp($2) }
| NOT expr     %prec UMINUS         
   { Not($2) }
| IF expr THEN expr ELSE expr       
   { Cond($2,$4,$6) }
| IDENT PARENT_G PARENT_D           
   { IdentVide($1) }
| IDENT PARENT_G arg_list PARENT_D  
   { IdentArgs($1,$3) }
| TRUE                
   { True }
| FALSE               
   { False }
| IDENT               
   { Var($1) }
| CSTE                
   { CsteEnt($1) }
| CHAINE              
   { Chaine($1) }
| expr SUP     expr   
   { Sup($1,$3) }
| expr SUPEGAL expr   
   { SupEgal($1,$3) }
| expr INF     expr   
   { Inf($1,$3) }
| expr INFEGAL expr   
   { InfEgal($1,$3) }
| expr EGAL    expr   
   { Egal($1,$3) }
| expr DIFF    expr   
   { Diff($1,$3) }
;

/*s Definition DEF */
def:
|  REC IDENT PARENT_G PARENT_D DEUX_POINTS t_type EGAL expr            
    { RecVide($2,$6,$8) }

| REC IDENT PARENT_G decl_list PARENT_D DEUX_POINTS t_type EGAL expr  
    { RecArgs($2,$4,$7,$9) }

| IDENT PARENT_G PARENT_D DEUX_POINTS t_type EGAL expr                
    { FuncVide($1,$5,$7) }

| IDENT PARENT_G decl_list PARENT_D DEUX_POINTS t_type EGAL expr      
    { FuncArgs($1,$3,$6,$8) }

| IDENT DEUX_POINTS t_type EGAL expr                                       
    { Id($1,$3,$5) }
;

/*s Definition [DECL_LIST] */
decl_list:
|   IDENT DEUX_POINTS t_type VIRGULE decl_list  
    { ListDecl($1,$3,$5) }
|  IDENT DEUX_POINTS t_type
    { Decl($1,$3) }

;

/*s Definition [ARG_LIST] */
arg_list:
| expr                     
    { Arg($1)        }
| expr VIRGULE arg_list 
    { ArgList($1,$3) }
;

/*s Definition [T_TYPE] */
t_type:
|  INT        
    { Int    }
| BOOL    
    { Bool   }
| STRING   
    { String }
;




