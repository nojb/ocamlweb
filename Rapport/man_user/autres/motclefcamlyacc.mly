/* Fichier motclefcamlyacc.mly 
   C'est un test avec des mot-clefs caml utilises dans les parties yacc 
   Ces mot-clefs sont consideres comme des identifiants
*/


%{
  open String 

  let affiche i =  print_string (string_of_int i)

%}

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token IF

%left PLUS MINUS  
%left TIMES DIV   
%nonassoc UMINUS  

%start main            
%type <int> while           

%%

while:
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

