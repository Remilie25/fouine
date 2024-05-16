(* un type pour des expressions *)
type expr =
    Const of int 
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | Div of expr*expr
  | Eq of expr*expr
  | Grt of expr*expr
  (*| Geq of expr*expr*)

  | Bool of bool
  | Or of expr*expr
  | And of expr*expr
  | Not of expr
  | If_then_else of expr*expr*expr

  | Id of string
  | Let_id_in of expr*expr*expr
  | Fun of expr*expr
  | App of expr*expr


(*Definition du type valeur et de ses operations*)

type valeur = Vc of int | Vb of bool | Vf of expr*expr;;

exception NotAllowedOperation

let default_equal s t = s=t;;
        
let ( + ) a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k+l)
  (*|Vb(k),Vb(l) -> Vb(k||l)*)
  |_,_ -> raise NotAllowedOperation;;

let ( * ) a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k*l)
  (*|Vb(k),Vb(l) -> Vb(k&&l)*)
  |_,_ -> raise NotAllowedOperation;;

let ( - ) a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k-l)
  |_,_ -> raise NotAllowedOperation;;

let ( / ) a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k/l)
  |_,_ -> raise NotAllowedOperation;;

let ( = ) a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k=l)
  |Vb(k),Vb(l) -> Vb(k=l)
  |_,_ -> raise NotAllowedOperation;; (*never used because case before*)

let ( > ) a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k>l)
  |_,_ -> raise NotAllowedOperation;;

let ( || ) a b = match a,b with
  |Vb(k),Vb(l) -> Vb(k||l)
  |_,_ -> raise NotAllowedOperation;;

let ( && ) a b = match a,b with
  |Vb(k),Vb(l) -> Vb(k&&l)
  |_,_ -> raise NotAllowedOperation;;

let not a = match a with
  |Vb(k) -> Vb(not(k))
  |_ -> raise NotAllowedOperation;;

let bool_of_valeur a = match a with
  |Vb(b) -> b
  |_ -> raise NotAllowedOperation;;

let print_bool b =
  if b then print_string "true"
  else print_string "false";;

let print_valeur a = match a with
  |Vc(k) -> print_int(k)
  |Vb(b) -> print_bool(b)
  |Vf(_,_) -> print_string "fun"

let test =
  [Vc(1) + Vc(2);
  Vc(2) * Vc(3);
  Vc(12) - Vc(3);
  Vc(24) / Vc(2);
  Vc(1) = Vc(2);
  Vc(3) = Vc(3);
  Vb(true) = Vb(false);
  Vb(true) = Vb(true);
  Vc(1) > Vc(2);
  Vc(4) > Vc(2);
  Vb(false) || Vb(false);
  Vb(true) || Vb(false);
  Vb(true) && Vb (false);
  Vb(true) && Vb(true);
  not(Vb(true));
  not(Vb(false));
  Vb(default_equal "abc" "defg");
  Vb(default_equal "brw" "brw")];;

(* Definition du type environemment *)

type env = string*valeur list
