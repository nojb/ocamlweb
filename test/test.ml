(* header 1 *)
(* header 2 *)

(*i $Id$ i*)

(*i*)
open Toto
(*i*)

(* documentation $\beta$ 1 *)

type truc = AA | BB | CC

exception 'a AAA of 'a aatoto * 'b aatruc * aamachin

type machin = { champ1 : int;
		champ2 : string }

let (C (x,y)) = x != y

(*s Voila un commentaire dans lequel je cite du code : *)
(*i(*i*)

    let f x = x + 1
    let rec g x = if x = 0 then 1 else g (f (x-1))

(*i*)i*)
(* et voila c'est fait ! *)

let const = 0X4fff + 0O455 + 0B01010111001 + 1.5152E-4
let autrec = 0xafaf125
let f x = x land 0x4fffff

let f toto toto' = c := !c + 8

(*s documentation 2 où je parle de [ma_fonction x] ci-dessous définie
    et de bien d'autres choses que j'aimerais voir s'afficher sur un
    paragraphe un tant soit peu grand, comme celui-ci. 
    Je peux aussi parler de \verb!@x! si je veux... 
    ou encore << échapper >> cette ["chaine de caractères"]. *)

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

(*s un vrai commentaire *)

let autre_bout x =
  if x <= 0 then x+1 else x+2;          (*r un vrai commentaire *)
  f 3;                                  (*r et hop *)
  cool ()                               (*r et voilà une troisième ligne *)

let chaine =
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
