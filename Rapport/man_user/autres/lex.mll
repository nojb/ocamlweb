(* Fichier lex.mll 
   C'est un test de base sans commentaires  
*)
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
  let x=3 in x+2;;

}
