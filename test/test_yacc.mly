/* ON NE VEUT PAS VOIR CE COMMENTAIRE */

/*s Bonjour */

%{

  (*s Bla bla bla *)

  type tagada = int * int

  let la_fonction_du_header x = x

%}

/*s Tokens. */

%token PLUS MOINS MULT DIV
%token <string> IDENT

%type <tagada -> bar> main
%start main

%%

/*s Rules. */

main:
   ffff IDENT gggg { la_fonction_du_header Action }
;

ffff:
   ffff IDENT gggg { Action }
;

%%

(*s Et hop... *)
  
type ttt = tagada
	     
let ff x = x
	     
let gg = la_fonction_du_header ff

