/* Fichier motclefyacccaml.mly 
   C'est un test avec des mot-clefs yacc utilises dans les parties caml 
   Ces mot-clefs sont consideres comme des identifiants
*/


%{
  open String 

  let error i =  print_string (string_of_int i)

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
    INT                     { let left=6 in  $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | MINUS expr %prec UMINUS { - $2 }
  ;

%%

let error=3 in x+23;;


