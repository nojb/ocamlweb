(* Fichier lexcomheader.mll 
   C'est un test avec des commentaires dans l'entete
   Le controle séffectue correctement *)

{
  (*s entete du fichier *)
  open Yacccalc (*r justifie a droite *)
  (* documentation *)  
  exception Eof

  let rec fact = function
    | 1 -> 1   (*c cas de base *)
    | n -> n * fact (n-1)
  (*i  commentaire ignore i*)
    
}

rule token = parse
    [' ' '\t']     { token lexbuf }     
  | ['\n' ]        { EOL }
  | ['0'-'9']+     { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }

{
  let x=3 in x+2;;

}
