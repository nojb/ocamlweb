/* Fichier parsecomheader.mly 
   C'est un test avec des commentaires dans l'entete
   Le controle séffectue correctement */


%{
 (*s entete du fichier *)
  open String  (*r justifie a droite *)
  (* documentation *)  
  let affiche i =  print_string (string_of_int i) (*c commentaire de code *)
 (*i  commentaire ignore i*)
 
%}

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS  
%left TIMES DIV   
%nonassoc UMINUS  

%start main            
%type <int> main           

%%

main:
    expr EOL                { $1 }
  ;
  
expr:
    INT                     { $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | MINUS expr %prec UMINUS { - $2 }
  ;

%%

let x=3 in x+23;;



