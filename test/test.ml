(* header 1 *)
(* header 2 *)

(*i $Id$ i*)

(*i*)
open Toto
(*i*)

(* Constantes num�riques. *)

let hexa = 0X4fff 
let hexa = 0x12BC 
let octal = 0O455 
let octal = 0o455 
let binary = 0B01010111001 
let binary = 0b00101011000
let float = 1.5152E-4
let float = 1.5152e-4

(*s Documentation 1 *)

type truc = AA | BB | CC

exception AAA of 'a aatoto * 'b aatruc * aamachin

type machin = { champ1 : int;
		champ2 : string }

let (C (x,y)) = x != y

(*s documentation 2 o� je parle de [ma_fonction x] ci-dessous d�finie
    et de bien d'autres choses que j'aimerais voir s'afficher sur un
    paragraphe un tant soit peu grand, comme celui-ci. 
    Je peux aussi parler de \verb!@x! si je veux... 
    ou encore << �chapper >> cette ["chaine de caract�res"]. *)

let l = 
  [|[|1;3;2|];
    [|4;5;6|]|]

let t = 1 ** 2

let une_grande_chaine = "tatdn jgj gjhg jfjh \
    kjhsljkh hglg lglj gljb lgjh \
    skjhk hmkjhmlkhj mkjl kj"

let une_autre_grande_chaine = "tatdn jgj gjhg jfjh
    kjhsljkh hglg lglj gljb lgjh
    skjhk hmkjhmlkhj mkjl kj"

let ma_fonction x = function (yyy,z) ->
  x + yyy*z (* commentaire : $y\not=0$ *) + toto

let s c = "\\" ^ c ^ "autre chaine"

let test x y z = x or y && not x = y

let test xor ory = xor + (*c commentaire *) ory

let xxx = { champ1 = 1; champ2 = "toto" }

let yyy = [| 1; xxx; 3 |]

(* un vrai commentaire *)

let autre_bout x =
  if x <= 0 then x+1 else x+2;          (*r un vrai commentaire *)
  f 3;                                  (*r et hop *)
  cool ()                               (*r et voil� une troisi�me ligne *)

let chaine =
  "(* une grande chaine
   qui se poursuit sur deux lignes"; while true do () done;
  '"' + "une autre chaine"
  '\134' + '\n' + toto

(*    documentation 3 : cette fonction a pour effet de mettre dans la 
  r�f�rence [x1] la valeur [x2]. *)

let mon_autre_fonction x1 x2 =
  x1 := x2;
  ma_fonction !x1 (x2,x2/3)

let toto = match truc with tagada -> ma_fonction champ1

let test_pat = function
    A1 -> 1
  | BB -> toto
  | CC -> CC.f

(* *)

let y = tagada

(* *)

let x = toto
