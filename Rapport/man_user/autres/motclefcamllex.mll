(* Fichier motclefcamllex.mll 
   C'est un test avec des mot-clefs caml utilises dans les parties lex 
   Ces mot-clefs sont consideres comme des identifiants
*)

{

  open Yacccalc 
  exception Eof
}

rule if = parse
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
