(*fichier de test*)

{
  open Var;;
  open Exceptions;;
  open Parser;;        (* Le type token est defini dans le module parser.mli *)
  open Printf;;


(*s Pour l'option -dump-tokens *)
let rec print_token = function
  | EOF         -> printf "EOF"
  | PLUS        -> printf "PLUS ; "
  | MOINS       -> printf "MOINS ; "
  | MULT        -> printf "MULT ; "
  | DIV         -> printf "DIV ; "
  | PARENT_G    -> printf "PARENT_G ; "
  | PARENT_D    -> printf "PARENT_D ; "
  | IDENT(s)    -> printf "IDENT(%s) ; " s
  | CSTE(n)     -> printf "CSTE(%d) ; " n
  | CHAINE(s)   -> printf "CHAINE(%s) ; " s
  | TRUE        -> printf "TRUE ; "
  | FALSE       -> printf "FALSE ; "
  | LET         -> printf "LET ; "
  | NOT         -> printf "NOT ; "
  | IN          -> printf "IN ; "
  | REC         -> printf "REC ; "
  | IF          -> printf "IF ; "
  | THEN        -> printf "THEN ; "
  | ELSE        -> printf "ELSE ; "
  | INT         -> printf "INT ; "
  | BOOL        -> printf "BOOL ; "
  | STRING      -> printf "STRING ; "
  | VIRGULE     -> printf "VIRGULE ; "
  | DEUX_POINTS -> printf "DEUX_POINTS ; "
  | ET          -> printf "ET ; "
  | OU          -> printf "OU ; "
  | EXP         -> printf "EXP ; "
  | SUP         -> printf "SUP ; "
  | SUPEGAL     -> printf "SUPEGAL ; "
  | INF         -> printf "INF ; "
  | INFEGAL     -> printf "INFEGAL ; "
  | EGAL        -> printf "EGAL ; "
  | DIFF        -> printf "DIFF ; "
  | LIFULA      -> printf "LIFULA ; "
;;


(*s Crée une table de hachage pour les tokens, cela a pour effet de réduire énormément 
 l'automate de l'analyse lexicale. *)
let keyword_table = Hashtbl.create 29

let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ 
      "let"              , LET    ;
      "not"              , NOT    ;
      "in"               , IN     ;
      "rec"              , REC    ;
      "if"               , IF     ;
      "then"             , THEN   ;
      "else"             , ELSE   ;
      "int"              , INT    ;
      "bool"             , BOOL   ;
      "string"           , STRING ;
      "not"              , NOT    ;
      "true"             , TRUE   ;
      "false"            , FALSE  ;
      "lifula"           , LIFULA ;
    ]
}


(*s Définitions de variables pour la lisibilité du lexer *)
let blanc      = [' ' '\n' '\t']
let lettre_min = ['a'-'z']
let lettre_maj = ['A'-'Z']
let lettre     = lettre_min | lettre_maj
let chiffre    = ['0'-'9']
let mot_cle    = lettre (lettre | chiffre | '_'| '\'')*
let chaine     = '"' ([^'"' '\\'] | "\\n" | "\\t" | "\\\\" | "\\\"")* '"'


(*s Lexer 1 *)
(*
  Analyseur lexical. 
  Si la chaine est du type [mot_cle], on la cherche dans la table de 
  hachage. Si on ne trouve pas de mots-clé correspondant, on la considère comme 
  un identificateur. Sinon on analyse directement la chaine.
 *)
rule token = parse
    mot_cle
    { 
      let id = Lexing.lexeme lexbuf in
        (try
           Hashtbl.find keyword_table id
         with Not_found ->
           IDENT(id)) 
    }
  | blanc               
      {
 	if (Lexing.lexeme_char lexbuf 0 = '\n')
	then
	  begin
	    incr ligne  ;
	    car := Lexing.lexeme_end lexbuf ;
	  end ;
	  token lexbuf 
      }
  | "(*"                
      {
 	incr compteur  ; 
	comment lexbuf ;
	token lexbuf 
      }
  | ","                { VIRGULE     }
  | ":"                { DEUX_POINTS }
  | "="                { EGAL        }
  | "+"                { PLUS        }
  | "-"                { MOINS       }
  | "*"                { MULT        }
  | "/"                { DIV         }
  | "("                { PARENT_G    }
  | ")"                { PARENT_D    }
  | "^"                { EXP         }
  | "&"                { ET          }
  | "|"                { OU          }
  | ">"                { SUP         }
  | ">="               { SUPEGAL     }
  | "<"                { INF         }
  | "<="               { INFEGAL     }
  | "<>"               { DIFF        }
  | "!="               { DIFF        }
  | chiffre+  (* les chiffres négatifs seront reconnus lors de l'analyse syntaxique *)
      { 
	try 
	  CSTE(int_of_string(Lexing.lexeme lexbuf)) 
	with _ ->
          begin
	    debut := Lexing.lexeme_start lexbuf - !car + 1 ;
	    fin := Lexing.lexeme_end lexbuf - !car + 1 ;
	    let error = Lexer_error(!ligne , !debut , !fin , "Problème lié à l'entier")
 	    in raise error
	  end
      }
  | chaine              
      { 
	CHAINE(Lexing.lexeme lexbuf) 
      } 
  | eof      { EOF }   
  | _         
      {
 	debut := Lexing.lexeme_start lexbuf - !car + 1 ;
	fin := Lexing.lexeme_end lexbuf - !car + 1 ;
	let error = Lexer_error(!ligne , !debut , !fin , "token non reconnu") in raise error 
      }

and

(*s Lexer 2 *)
(* Reconnait et ignore les commentaires.Si on a autant de commentaires fermants 
   que d'ouvrants, on sort de toutes facons, le surplus de fermants eventuels etants 
   consideres comme MULT [PARENT_D]. *)
comment = parse
    chaine   
      { comment lexbuf }
  | "*)"
      {
        decr compteur      ;
        if (!compteur <> 0)
	then comment lexbuf
      }
  | "(*"
      {
        incr compteur ;
	comment lexbuf
      }
  | '\n'
      { 
	incr ligne  ;
	car := Lexing.lexeme_end lexbuf ;
	comment lexbuf
      }
  | eof 
      {
        debut := Lexing.lexeme_start lexbuf - !car + 1 ;
	fin := Lexing.lexeme_end lexbuf - !car + 1 ;
	let error = Lexer_error(!ligne , !debut , !fin , "commentaire fermant attendu") in
	  raise error }
  | _  
      { comment lexbuf }
