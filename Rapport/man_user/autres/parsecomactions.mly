/* Fichier parsecomactions.mly 
   C'est un test avec des commentaires dans les actions
   Le controle s'effectue correctement */


%{
  open String  

  let affiche i =  print_string (string_of_int i) 
 
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
    expr EOL                { $1 (*c commentaire de code *) }
  ;
  
expr:
    INT                     
    {
      (*s une action *)
      $1 
    }
  | LPAREN expr RPAREN      
      { 
       (* une autre action *)  
	$2 
      }
  | expr PLUS expr          
      { 
	$1 + $3 (*r justifie a droite *)
      }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { (*i  commentaire ignore i*) $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | MINUS expr %prec UMINUS { - $2 }
  ;

%%

let y = true ;; 

let x=3 in x+23;;



