(* Fichier motcleflexcaml.mll 
   C'est un test avec des mot-clefs lex utilises dans les parties caml 
   Ces mot-clefs sont consideres comme des identifiants
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
  | '('            { let parse = "true" in LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }

{
  let rule=3 in x+2;;

}
