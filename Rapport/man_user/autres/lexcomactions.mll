(* Fichier lexcomactions.mll
   C'est un test avec des commentaires dans les actions
   Le controle s'effectue correctement *)

{

  open Yacccalc 
  exception Eof
}

rule token = parse
    [' ' '\t']     
    { 
      (*s premiere action*)
      token lexbuf 
    }     
  | ['\n' ]        
      { 
      (* seconde action *)
	EOL (*r justifie a droite *) 
      }
  | ['0'-'9']+     
    {
      (* troisieme action *)
      INT(int_of_string(Lexing.lexeme lexbuf)) 
    }
  | '+'            
      { 
	(*i ignore i*)
	PLUS 
      }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            
      { 
	raise Eof
        (*c exception levee *)
      }
      
{
  let x=3 in x+2;;

}
