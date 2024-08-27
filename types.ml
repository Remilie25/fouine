(* Definition du type rec_state. Indique si une fonction est rec ou non et stocke son id si elle l'est.*)
type rec_state = Non_rec | Rec of string;;

(* un type pour des expressions *)  
type expr =
    Const of int
  | Bool of bool

  | Unary_op of (valeur -> valeur) term
  | Binary_op of (valeur -> valeur -> valeur) term

  | If_then of expr * expr
  | If_then_else of expr * expr * expr

  | Id of string
  | Let_id_in of rec_state * expr list * expr * expr
  | Fun of rec_state * expr list * expr
  | App of expr * expr
  | Expr_unit

  | Let_id of rec_state * expr list * expr



(*Definition du type valeur et de ses operations*)

and valeur =
    Val_unit
  | Vc of int
  | Vb of bool
        
  (*fouine fun*)
  | Vf of rec_state * expr * expr * ((string * valeur) list)

  (*OCaml fun*)
  | Vf_una of (valeur -> valeur)
  | Vf_bin of (valeur -> valeur -> valeur)

and _ term=
  | Add  : (valeur -> valeur -> valeur) term
  | Mul  : (valeur -> valeur -> valeur) term
  | Min  : (valeur -> valeur -> valeur) term
  | Div  : (valeur -> valeur -> valeur) term
  
  | Eq   : ('a -> 'a -> valeur) term
  | Neq  : ('a -> 'a -> valeur) term
  | Grt  : ('a -> 'a -> valeur) term
  | Geq  : ('a -> 'a -> valeur) term
  | Less : ('a -> 'a -> valeur) term
  | Leq  : ('a -> 'a -> valeur) term
         
  | Or   : (valeur -> valeur -> valeur) term
  | And  : (valeur -> valeur -> valeur) term
  | Not  : (valeur -> valeur) term
;;

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
  (*Msg OCaml d'origine """Exception: Invalid_argument "compare: functional value"."""*)
  (*Pas besoin de faire tous les couples possibles car si l'evaluation a lieu
    l'expression est typable donc c'est bien un couple de fun de meme type.*)
  |Vf(_,_,_,_) , _ | Vf_una _ , _ |Vf_bin _ , _ -> raise NotAllowedOperation
  |_,_ -> raise NotAllowedOperation;;

let val_neq a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k<>l)
  |Vb(k),Vb(l) -> Vb(k<>l)
  |Vf(_,_,_,_) , _ | Vf_una _ , _ |Vf_bin _ , _ -> raise NotAllowedOperation
  |_,_ -> raise NotAllowedOperation;;

let val_grt a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k>l)
  |_,_ -> raise NotAllowedOperation;;

let val_geq a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k>=l)
  |_,_ -> raise NotAllowedOperation;;

let val_less a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k<l)
  |_,_ -> raise NotAllowedOperation;;

let val_leq a b = match a,b with
  |Vc(k),Vc(l) -> Vb(k<=l)
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
  | Val_unit -> print_string "unit = ()"
  | Vc(k) -> print_string "int = "; print_int k
  | Vb(b) -> print_string "bool = "; print_bool b
  | Vf(_,_,_,_) -> print_string "fun"
  | Vf_una _ -> print_string "fun"
  | Vf_bin _ -> print_string "fun";;

let print_result ele =
  if (fst ele) = "-" then print_string (fst ele)
  else (print_string "val "; print_string (fst ele));
  print_string " : "; print_valeur (snd ele); print_newline ()
;;

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
