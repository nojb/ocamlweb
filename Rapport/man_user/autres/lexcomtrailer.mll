(* Fichier lexcomtrailer.mll
   C'est un test avec des commentaires dans l'enqueue
   Le controle s'effectue correctement *)

{

  open Yacccalc 
  exception Eof
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
  (*s enqueue du fichier *)
  let x=3 in x+2(*r justifie a droite *)

  (* documentation *)  
  let rec fact = function
    | 1 -> 1   (*c cas de base *)
    | n -> n * fact (n-1)
  (*i  commentaire ignore i*)
    
}
