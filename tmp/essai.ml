(* header 1
 * header 2 *)

(* $Id$ *)

(* documentation $\beta$ 1 *)

type 'oper term =
  | DOP0 of 'oper                            (* atomic terms *)
  | DOP1 of 'oper * 'oper term               (* operator of arity 1 *)
  | DOP2 of 'oper * 'oper term * 'oper term  (* operator of arity 2 *)
  | DOPN of 'oper * 'oper term array         (* operator of variadic arity *)
  | DOPL of 'oper * 'oper term list          (* operator of variadic arity *)
  | DLAM of name * 'oper term                (* deBruijn binder on one term*)
  | DLAMV of name * 'oper term array         (* deBruijn binder on many terms*)
  | VAR of identifier                        (* named variable *)
  | Rel of int                               (* variable as deBruijn index *)

(*s [(closedn n M)] raises [FreeVar] if a variable of height greater than [n]
   occurs in [M], returns () otherwise *)

let closedn = let rec closed_rec n = function
  | Rel(m) -> if m>n then raise FreeVar
  | VAR _ -> ()
  | DOPN(_,cl) -> Array.iter (closed_rec n) cl
  | DOPL(_,cl) -> List.iter (closed_rec n) cl
  | DOP2(_,c1,c2) -> closed_rec n c1; closed_rec n c2
  | DOP1(_,c) -> closed_rec n c
  | DLAM(_,c) -> closed_rec (n+1) c
  | DLAMV(_,v) -> Array.iter (closed_rec (n+1)) v
  | _ -> ()
in closed_rec

(*s documentation 2 où je parle de [ma_fonction x] ci-dessous définie
    et de bien d'autres choses que j'aimerais voir s'afficher sur un
    paragraphe un tant soit peu grand, comme celui-ci. 
  Je peux aussi parler de \verb!@x! si je veux... *)

let ma_fonction x = function (y,z) ->
  x + y*z (* commentaire : $[y]\not=0$ *)

let autre_bout x =
  if x <= 0 then x+1 else x+2;
  bal bla x if y' if z123'_45;
    while true do () done

(*s    documentation 3 : cette fonction a pour effet de mettre dans la 
  référence [x1] la valeur [x2]. *)

let mon_autre_fonction x1 x2 =
  x1 := x2
