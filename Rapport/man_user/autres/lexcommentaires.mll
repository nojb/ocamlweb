(* Fichier lexcommentaires.mll 
   C'est un test avec des commentaires dans le code yacc 
   Le controle s'effectue correctement *)

{

  open Yacccalc 
  exception Eof
}


(*s Actions *)
rule token = parse
    [' ' '\t']  (* documentation1 *)   { token lexbuf }     
  | ['\n' ]        { EOL } (*r justifie a droite *)
  | ['0'-'9']+     { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | '+'       (*c action plus *)    { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  (* Documentation2*)
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }   (*i ignore i*)
  | eof            { raise Eof }

{
  let x=3 in x+2;;

}
