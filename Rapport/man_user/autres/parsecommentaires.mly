/* Fichier parsecommentaires.mly 
   C'est un test avec des commentaires dans le code yacc 
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

/*s une regle */
main:
/*c commentaire de code */
    expr EOL                { $1  }
  ;
  
/* une autre regle */  
expr:
    INT                     
    {
      $1 
    }
  | LPAREN expr RPAREN      
      { 
	$2 
      }
  | expr PLUS expr /*r justifie a droite */         
      { 
	$1 + $3 
      }
  | expr MINUS expr       /*c commentaire de code */  { $1 - $3 }
  | expr TIMES expr  /*i  commentaire ignore i*/        { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
      /*i  commentaire ignore i*/
  | MINUS expr %prec UMINUS { - $2 }
  ;

%%

let y = true ;; 

let x=3 in x+23;;



