(* header 1
 * header 2 *)

(* $Id$ *)

(* documentation $\beta$ 1 *)

(*s documentation 2 où je parle de [ma_fonction x] ci-dessous définie
    et de bien d'autres choses que j'aimerais voir s'afficher sur un
    paragraphe un tant soit peu grand, comme celui-ci. 
  Je peux aussi parler de \verb!@x! si je veux... *)

let ma_fonction x = function (y,z) ->
  x + y*z (* commentaire : $[y]\not=0$ *)

let autre_bout x =
  if x <= 0 then x+1 else x+2;
  while true do () done

(*s    documentation 3 : cette fonction a pour effet de mettre dans la 
  référence [x1] la valeur [x2]. *)

let mon_autre_fonction x1 x2 =
  x1 := x2;
  ma_fonction !x1 (x2,x2/3)

