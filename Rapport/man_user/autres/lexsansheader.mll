/* Fichier lexsansheader.mll 
   C'est un test de base sans commentaires et sans entete 
   Tout s'affiche comme prevu */

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
