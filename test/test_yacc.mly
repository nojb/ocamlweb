/* ON NE VEUT PAS VOIR CE COMMENTAIRE */

/*s Bonjour */

%{

  (*s Bla bla bla *)

  type tagada = int * int

%}

/*s Tokens. */

%token PLUS MOINS MULT DIV
%token <string> IDENT

%type <tagada> main
%start main

%%

/*s Rules. */

main:
   ffff IDENT gggg { Action }
;

%%

/*s Et hop... */
  
type ttt = tagada
	     
let ff x = x
	     
let gg = ff
