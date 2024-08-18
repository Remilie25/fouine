open Types

exception ToDo of string
exception NotAFunction of int
exception NotExpectedType

(* fonction d'affichage *)
let rec affiche_expr e =
  let rec aff_aux s sl = match sl with
    | [] -> print_string ")"
    | x::r -> (print_string s; affiche_expr x; aff_aux ", " r)
  in
  match e with
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" [e1;e2]
  | Mul(e1,e2) -> aff_aux "Mul(" [e1;e2]
  | Min(e1,e2) -> aff_aux "Min(" [e1;e2]
  | Div(e1,e2) -> aff_aux "Div(" [e1;e2]
  | Eq(e1,e2) -> aff_aux "Eq(" [e1;e2]
  | Grt(e1,e2) -> aff_aux "Grt(" [e1;e2]
                
  | Bool b -> print_bool b
  | Or(e1,e2) -> aff_aux "Or(" [e1;e2]
  | And(e1,e2) -> aff_aux "And(" [e1;e2]
  | Not(e1) -> print_string "Not("; affiche_expr e1; print_string ")"
  | If_then(e1,e2) -> aff_aux "If_then(" [e1;e2]
  | If_then_else(e1,e2,e3) -> aff_aux "If_then_else("  [e1;e2;e3]

  | Id(s) -> print_string ( "Id(" ^ s ^ ")" )
  | Let_id_in(p,e1,e2) ->
     begin
       match p with
       | [s] -> aff_aux "Let_id_in(" [s;e1;e2]
       | fun_id::para -> (print_string "Let_id_in("; affiche_expr (fun_id); print_string ", ";
                          aff_fun (Fun(para, e1));
                          aff_aux ", " [e2])
       | _ -> raise (ToDo "msg err 0")
     end
    
  | Fun(_,_) -> aff_fun e
  | App(e1,e2) -> aff_aux "App(" [e1;e2]
  | Expr_unit -> print_string "Unit"

  and aff_fun f =
    match f with
    (*|Fun([],e) -> print_string ("Fun((),"); affiche_expr e; print_string ")" Cas inutile*)
    | Fun(para,e) -> 
      begin
        let rec aux l=match l with
          | [] -> affiche_expr e
          | x::r ->
             match x with
             | Id s -> ( print_string ("Fun("^s^", "); aux r; print_string ")" )
             | Expr_unit -> ( print_string ("Fun(Unit, "); aux r; print_string ")" )
             | _ -> raise (ToDo "msg err 1")    
        in aux para
      end

    | Id(_) | App(_,_) -> affiche_expr f
            
    |_-> raise (NotAFunction 0)
;;

(* Ce n'est pas judicieux dans cet ordre => il y a mieux a faire avant !
let aff_vf_type f=
  (* Cette fonction affiche le type d'une fonction dans le type valeur (Vf). *)
  (* WIP + copier dans aff_fun pour qd pas transfo. *)
  match f with
  | Vf(para, op, arg) ->
     let rec find_parameter_type e para_type tmp_type=
       match e with
         |

  | _ -> raise (NotAFunction 0)
;;*)

let is_id v x = match x with
  |(s,_) -> s = v;;

let rec cat_sans_doublons is_member list=
  let rec aux l1 l2 = match l2 with
    (*Parcourt l'env2 en ajoutant les id dans env1 sans doublons.*)
    | [] -> l1
    | ele::r when List.exists (is_member ele) l1 -> aux l1 r
    | ele::r -> ele :: (aux l1 r)
  in
  match list with
  | [l] -> l
  | [] -> []
  | l::_::_ -> aux (cat_sans_doublons is_member (List.tl list)) l;; 

let env_cat env_list =
  (*Cette fonciton concatene tous les environnements sans doublons.
    Les env sont supposes sans doublons*)
  cat_sans_doublons (fun ele couple -> (fst ele) = (fst couple)) env_list
  
and id_cat string_list =
  (*Cette fonction concatene des listes de string sans doublons.*)
  cat_sans_doublons (fun ele couple -> ele = couple) string_list;;


                      
(* s�mantique op�rationnelle � grands pas *)
let rec eval e envi = match e with
  | Const k -> Vc(k)
  | Add(e1,e2) -> val_add (eval e1 envi) (eval e2 envi)
  | Mul(e1,e2) -> val_mul (eval e1 envi) (eval e2 envi)
  | Min(e1,e2) -> val_min (eval e1 envi) (eval e2 envi)
  | Div(e1,e2) -> val_div (eval e1 envi) (eval e2 envi)
  | Eq(e1,e2) -> val_eq (eval e1 envi) (eval e2 envi)
  | Grt(e1,e2) -> val_grt (eval e1 envi) (eval e2 envi)
                
  | Bool b -> Vb(b)
  | Or(e1,e2) -> val_or (eval e1 envi) (eval e2 envi)
  | And(e1,e2) -> val_and (eval e1 envi) (eval e2 envi)
  | Not(e1) -> val_not(eval e1 envi)
  | If_then(e1,e2) ->begin
      if bool_of_valeur(eval e1 envi) then raise (ToDo "eval_unit ou unit dans eval");
      affiche_expr e2; (*pour ne pas avoir le warning tant que c'est WIP*)
      Val_unit
      end
  | If_then_else(e1,e2,e3) -> if bool_of_valeur(eval e1 envi) then (eval e2 envi) else (eval e3 envi)

  | Id(s) -> snd( List.find (is_id s) envi )

  | Expr_unit -> Val_unit
           
  |Let_id_in(p, e1, e2) ->
    begin
      match p with
      | [x] -> eval e2 ((string_of_id x, eval e1 envi)::envi)
      | fun_id::para -> eval e2 (( string_of_id fun_id, eval (Fun(para, e1)) envi )::envi)
      | _ -> raise (ToDo "msg err 2")
    end
    
  | Fun(para, op) ->
     let rec init_envi_local para op envi_local=
       (*Cette fonction construit la cloture de la fonction.
         para : liste des id a ne pas stocker. Cas ou c'est un parametre de la fonciton
                mais aussi apres la declaration d'un nouveau id.
         op : expression a parcourir pour trouver des id.
         envi_local : c'est la cloture.*)
       
       match op with
       (*Cas ou il faut faire grandir notre envi_local*)
       | Id(s) when List.mem s para -> envi_local

       | Id(s) -> ( s, eval op envi )::envi_local

                
       (*Pour garder les bonnes valuers. Ex ou il pourraient y avoir des pb sans :
         let k = 1 in let fun_test x= let aux l= k+l in aux x in let k = 2 in fun_test k
         A mettre dans les tests par la suite.*)
       | Fun(para2, op2) -> init_envi_local (id_cat [para;List.map string_of_id para2]) op2 []
       
       | Let_id_in(x,e1,e2) ->
          begin
            match x with
            (*Ici on definit une nouvelle valeur a un id donc pas besoin de stocker son eventuelle 
              valeur anterieure si l'on tombe sur id apres le "in". En revanche dans la def de sa
              nouvelle valeur, nous en avons besoin.*)
            | [x] -> env_cat [init_envi_local para e1 envi_local;
                              init_envi_local ((string_of_id x)::para) e2 envi_local]

            (*Ici on definit une nouvelle fonction donc pas besoin de stocker l'eventuelle valeur
              anterieure si l'on tombe sur id apres le "in". Les parametres oui car se ne sont plus
              des parametres apres la definition. En revanche dans la def de la fonction, nous en 
              avons besoin mais pas les valeurs des parametres.*)
            | fun_id::para2 ->
               env_cat [init_envi_local (id_cat [para;List.map string_of_id para2]) e1 envi_local;
                        init_envi_local [string_of_id fun_id] e2 envi_local]
                                
            | _ -> raise (ToDo "msg err 3")
          end
         
            
       (*Le reste : appel rec ou cas de base sans faire grandir l'envi_local*)
       | Const _ | Bool _ | Expr_unit-> envi_local

       | Not(e1) -> init_envi_local para e1 envi_local
                           
       | Add(e1,e2) | Mul(e1,e2) | Min(e1,e2) | Div(e1,e2) | Eq(e1,e2) | Grt(e1,e2) | Or(e1,e2)
         | And(e1,e2) | If_then(e1,e2) | App(e1,e2)
         -> env_cat [init_envi_local para e1 envi_local; init_envi_local para e2 envi_local]
         
       | If_then_else(e1,e2,e3) -> env_cat [init_envi_local para e1 envi_local;
         init_envi_local para e2 envi_local; init_envi_local para e3 envi_local]
     in
     begin
     match para with
     | [p0] -> Vf(p0, op, init_envi_local (List.map string_of_id para) op [])
     | p0::_::_ -> Vf(p0, Fun(List.tl para, op), init_envi_local (List.map string_of_id para) op [])
     | [] -> raise (NotAFunction 2)
     end

  | App(f, arg) ->

     match eval f envi with
       
     | Vf(para, op, envi_local) ->
        begin
          match para with
          | Expr_unit -> if (eval arg envi) = Val_unit then eval op envi_local
                         else raise NotExpectedType
                       
          | Id s -> eval op ((s, eval arg envi)::envi_local)

          | _ -> raise (NotAFunction 2)
        end
       
     | _ -> raise (NotAFunction 1)
