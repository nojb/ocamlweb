/*

 Gros Blabla

*/

/*i $Id$ i*/

/*s Header Yacc en \LaTeX\ */
%{
  open Exceptions;;
  open Asa_expr;;

  let parse_error s =
    let error = 
      Parser_error(Parsing.symbol_start(),
		   Parsing.symbol_end(),
		   "" ) 
    in raise error
  ;;
(*s  Une petite section caml pour voir ... *)
  let warning = ref false;;
%}
%token EOF LIFULA
%token <end string->string(*mettre une fleche ici *)> IDENT

%right LET
%left IN 

/*s Une petite section pour voir 1... */

%nonassoc UMINUS

%start main

%type <Asa_expr.asa_expr> main

%%
/*s Une petite section pour voir 2... */


/*s Une petite section pour voir 3... */

main:
  LIFULA expr EOF  
       {  
	 let parse_error2 s =
	   let error = Parser_error(
	     Parsing.symbol_start(),
	     Parsing.symbol_end(),
	     "" ) 
	   in raise error }      
| expr EOF               
      { 
    	warning:=true;
	$1
      }
;
 /*c commentaire1 */
t_type:  /*c commentaire2 */
 /*c commentaire3 */
|  INT        
    { Int    }
| BOOL    
    { Bool   }
| STRING   
    { String }
;

%%

let parse_error3 s =
  let error = Parser_error(
    Parsing.symbol_start(),
    Parsing.symbol_end(),
    "" ) 
  in raise error
;;
