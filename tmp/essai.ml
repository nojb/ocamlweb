(* header 1 *)
(* header 2 *)

(* $Id$ *)

(* documentation $\beta$ 1 *)

type truc = AA | BB | CC

exception AAA of aatoto * aatruc * aamachin

type machin = { champ1 : int;
		champ2 : string }

(*s documentation 2 où je parle de [ma_fonction x] ci-dessous définie
    et de bien d'autres choses que j'aimerais voir s'afficher sur un
    paragraphe un tant soit peu grand, comme celui-ci. 
    Je peux aussi parler de \verb!@x! si je veux... 
    ou encore << échapper >> cette ["chaine de caractères"]. *)

let ma_fonction x = function (yyy,z) ->
  x + yyy*z (* commentaire : $[y]\not=0$ *) + toto

let s c = "\\" ^ c ^ "autre chaine"

let xxx = { champ1 = 1; champ2 = "toto" }

let yyy = [| 1; xxx; 3 |]

(*s un vrai commentaire *)

let autre_bout x =
  if x <= 0 then x+1 else x+2; (*c un vrai commentaire *)
  "(* une grande chaine
   qui se poursuit sur deux lignes"; while true do () done;
  '"' + "une autre chaine"
  '\134' + '\n' + toto

(*s    documentation 3 : cette fonction a pour effet de mettre dans la 
  référence [x1] la valeur [x2]. *)

let mon_autre_fonction x1 x2 =
  x1 := x2;
  ma_fonction !x1 (x2,x2/3)

let toto = match truc with tagada -> ma_fonction champ1

let test_pat = function
    A1 -> 1
  | BB -> toto
  | CC -> CC.f

(*s *)

let y = tagada

(*s *)

let x = toto
