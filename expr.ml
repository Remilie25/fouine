open Types

exception ToDo of string
exception NotAFunction

(* fonction d'affichage *)
let rec affiche_expr e =
  let aff_aux_2e s a b = 
    begin
      print_string s;
      affiche_expr a;
      print_string ", ";
      affiche_expr b;
      print_string ")"
    end
  in
  let aff_aux_3e s a b c = 
    begin
      print_string s;
      affiche_expr a;
      print_string ", ";
      affiche_expr b;
      print_string ", ";
      affiche_expr c;
      print_string ")"
    end
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux_2e "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux_2e "Mul(" e1 e2
  | Min(e1,e2) -> aff_aux_2e "Min(" e1 e2
  | Div(e1,e2) -> aff_aux_2e "Div(" e1 e2
  | Eq(e1,e2) -> aff_aux_2e "Eq(" e1 e2
  | Grt(e1,e2) -> aff_aux_2e "Grt(" e1 e2
                
  | Bool b -> print_bool b
  | Or(e1,e2) -> aff_aux_2e "Or(" e1 e2
  | And(e1,e2) -> aff_aux_2e "And(" e1 e2
  | Not(e1) -> print_string "Not("; affiche_expr e1; print_string ")"
  | If_then_else(e1,e2,e3) -> aff_aux_3e "If_then_else("  e1 e2 e3

  | Id(s) -> print_string ( "Id(" ^ s ^ ")" )
  | Let_id_in(e1,e2,e3) -> aff_aux_3e "Let_id_in(" e1 e2 e3
  | Fun(e1,e2) -> aff_aux_2e "Fun(" e1 e2
  | App(e1,e2) -> aff_aux_2e "App(" e1 e2

let is_id v x = match x with
  |s,_ -> default_equal s v;;
                      
(* sémantique opérationnelle à grands pas *)
let rec eval e envi = match e with
  | Const k -> Vc(k)
  | Add(e1,e2) -> (eval e1 envi) + (eval e2 envi)
  | Mul(e1,e2) -> (eval e1 envi) * (eval e2 envi)
  | Min(e1,e2) -> (eval e1 envi) - (eval e2 envi)
  | Div(e1,e2) -> (eval e1 envi) / (eval e2 envi)
  | Eq(e1,e2) -> (eval e1 envi) = (eval e2 envi)
  | Grt(e1,e2) -> (eval e1 envi) > (eval e2 envi)
                
  | Bool b -> Vb(b)
  | Or(e1,e2) -> (eval e1 envi) || (eval e2 envi)
  | And(e1,e2) -> (eval e1 envi) && (eval e2 envi)
  | Not(e1) -> vnot(eval e1 envi)
  | If_then_else(e1,e2,e3) -> if bool_of_valeur(eval e1 envi) then (eval e2 envi) else (eval e3 envi)

  | Id(s) -> snd( List.find (is_id s) envi )

  | Let_id_in(e1,e2,e3) ->
     begin
       match e1 with
       |Id(s) -> (eval e3 ( (s, eval e2 envi)::envi ) )
       |_ -> raise (ToDo "functions etc")
     end
    
  | Fun(e1,e2) -> Vf(e1, e2)
     (*begin
       match e1 with
       |Id(s) -> (Id(s), (eval e2))
     end*)
  | App(e1,e2) ->
    begin
      match (eval e1 envi) with
      |Vf(v,f) ->
        begin
          match v with
          |Id(s) -> (eval f ( (s, eval e2 envi)::envi ) )
          |_ -> raise (ToDo "plusieurs para ?")
        end
      
      |_ -> raise NotAFunction
    end
