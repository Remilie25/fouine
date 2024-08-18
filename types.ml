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
  | If_then of expr*expr
  | If_then_else of expr*expr*expr

  | Id of string
  | Let_id_in of expr list*expr*expr
  | Fun of expr list*expr
  | App of expr*expr
  | Expr_unit


(*Definition du type valeur et de ses operations*)

type valeur = Vc of int | Vb of bool | Vf of expr*expr*((string*valeur) list) | Val_unit;;

exception NotAllowedOperation
exception ToDo of string

        
let val_add a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k+l)
  |_,_ -> raise NotAllowedOperation;;

let val_mul a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k*l)
  |_,_ -> raise NotAllowedOperation;;

let val_min a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k-l)
  |_,_ -> raise NotAllowedOperation;;

let val_div a b = match a,b with
  |Vc(k),Vc(l) -> Vc(k/l)
  |_,_ -> raise NotAllowedOperation;;

let val_eq a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k=l)
  |Vb(k),Vb(l) -> Vb(k=l)
  |_,_ -> raise NotAllowedOperation;;

let val_grt a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k>l)
  |_,_ -> raise NotAllowedOperation;;

let val_or a b = match a,b with
  |Vb(k),Vb(l) -> Vb(k||l)
  |_,_ -> raise NotAllowedOperation;;

let val_and a b = match a,b with
  |Vb(k),Vb(l) -> Vb(k&&l)
  |_,_ -> raise NotAllowedOperation;;

let val_not a = match a with
  |Vb(k) -> Vb(not(k))
  |_ -> raise NotAllowedOperation;;

let bool_of_valeur a = match a with
  |Vb(b) -> b
  |_ -> raise NotAllowedOperation;;

let string_of_id x = match x with
  | Id s -> s
  | Expr_unit -> "Unit" (*Solution temporaire. Pas d'id de ce nom car commence par une majuscule.*)
  | _ -> raise (ToDo "msg err from types")

let print_bool b =
  if b then print_string "true"
  else print_string "false";;

let print_valeur a = match a with
  |Vc(k) -> print_int k
  |Vb(b) -> print_bool b
  |Vf(_,_,_) -> print_string "fun"
  |Val_unit -> print_string "unit";;

let test =
  [val_add (Vc(1)) (Vc(2));
  val_mul (Vc(2)) (Vc(3));
  val_min (Vc(12)) (Vc(3));
  val_div (Vc(24)) (Vc(2));
  val_eq (Vc(1)) (Vc(2));
  val_eq (Vc(3)) (Vc(3));
  val_eq (Vb(true)) (Vb(false));
  val_eq (Vb(true)) (Vb(true));
  val_grt (Vc(1)) (Vc(2));
  val_grt (Vc(4)) (Vc(2));
  val_or (Vb(false)) (Vb(false));
  val_or (Vb(true)) (Vb(false));
  val_and (Vb(true)) (Vb (false));
  val_and (Vb(true)) (Vb(true));
  val_not(Vb(true));
  val_not(Vb(false));];;

(* Definition du type environemment *)
(*type env = (string*valeur) list
  Les envi_local sont directement stockes dans la val de fonction.*)




(*Ce n'est plus le cas car complique pour rien. *)

(*
type ele_env = Id_env of string*valeur | Fun_env of string*valeur*(ele_env list);;
 *)

(*type env = ele_env list;; 
 C'est la definition du type environnement. Mais il est commente car sans constructeur, 
 Ocaml dira que le type est ele_env list au lieu de env. Or je n'ai pas rajouter de
 constructeur pour ne pas complique pour rien le code.
 Donc eval : expr -> ele_env list -> valeur mais moralement eval : expr -> env -> valeur*)
