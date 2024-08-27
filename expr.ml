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
  | Expr_unit -> print_string "Unit"
  | Const k -> print_int k
  | Bool b -> print_bool b

  | Unary_op op ->
     begin match op with
     | Not -> print_string "Not"
     | _ -> .
     end

  | Binary_op op ->
     begin match op with
     | Add  -> print_string "Add"
     | Mul  -> print_string "Mul"
     | Min  -> print_string "Min"
     | Div  -> print_string "Div"
     | Eq   -> print_string "Eq"
     | Neq  -> print_string "Neq"
     | Grt  -> print_string "Grt"
     | Geq  -> print_string "Geq"
     | Less -> print_string "Less"
     | Leq  -> print_string "Leq"
     | Or   -> print_string "Or"
     | And  -> print_string "And"
     | _ -> .
     end

  | If_then(e1,e2) -> aff_aux "If_then(" [e1;e2]
  | If_then_else(e1,e2,e3) -> aff_aux "If_then_else("  [e1;e2;e3]

  | Id(s) -> print_string ( "Id(" ^ s ^ ")" )
  | Let_id_in(recu,p,e1,e2) ->
     begin
       match p with
       | [s] -> aff_aux "Let_id_in(" [s;e1;e2]
       | fun_id::para -> (print_string "Let_id_in("; affiche_expr (fun_id); print_string ", ";
                          aff_fun (Fun(recu,para, e1));
                          aff_aux ", " [e2])
       | _ -> raise (ToDo "msg err 0")
     end
    
  | Fun(_,_,_) -> aff_fun e
  | App(e1,e2) -> aff_aux "App(" [e1;e2]

  | Let_id(recu,p,e) ->
     begin
       match p with
       | [s] -> aff_aux "Let_id(" [s;e]
       | fun_id::para -> (print_string "Let_id("; affiche_expr (fun_id); print_string ", ";
                          aff_fun (Fun(recu,para, e)))
       | _ -> raise (ToDo "msg err 0")
     end

  and aff_fun f =
    match f with
    | Fun(_, para, e) -> 
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

let rec affiche_expr_list l= match l with
  | [e] -> affiche_expr e
  | e::r -> affiche_expr e; print_string ";; "; affiche_expr_list r
  | _ -> raise (ToDo "Cas impossible 1")
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

let build_fun_value fun_other_val_case value =
  (*Cette fonction sert a construire les fonctions du type valeur qui peuvent etre recursives.
    L'idee est qu'a chaque fois qu'on evalue une fonction rec, on la met dans son envi_local avant
    toute autre operation. Cela afin de conserver l'envi_local dans l'etat qu'il etait lors de la
    definition.*)

  match value with
  | Val_unit | Vc _ | Vb _ | Vf_una _ | Vf_bin _ -> fun_other_val_case value

  (*Si la fun est rec => on ajoute l'ajoute dans son envi_local. Sinon rien de special*)
  | Vf(recu, para, op, envi_local) ->
     begin
       match recu with
       | Non_rec -> value
       | Rec s -> Vf(recu, para, op, (s,value)::envi_local)
     end;;

let fun_id_if_rec recu = match recu with
  | Non_rec -> []
  | Rec s -> [s];;


                      
(* s�mantique op�rationnelle � grands pas *)
let rec eval e envi = match e with
  | Expr_unit -> Val_unit
  | Const k -> Vc(k)
  | Bool b -> Vb(b)

  | Unary_op op->
     begin match op with
     | Not -> Vf_una val_not
     | _ -> .
     end

  | Binary_op op->
     begin match op with
     | Add  -> Vf_bin val_add
     | Mul  -> Vf_bin val_mul
     | Min  -> Vf_bin val_min
     | Div  -> Vf_bin val_div
     | Eq   -> Vf_bin val_eq
     | Neq  -> Vf_bin val_neq
     | Grt  -> Vf_bin val_grt
     | Geq  -> Vf_bin val_geq
     | Less -> Vf_bin val_less
     | Leq  -> Vf_bin val_leq
     | Or   -> Vf_bin val_or
     | And  -> Vf_bin val_and
     | _ -> .
     end

  | If_then(e1,e2) ->begin
      if bool_of_valeur(eval e1 envi)
      then begin
          match eval e2 envi with
          | Val_unit -> ()
          | _ -> raise (ToDo "msg err : this expression should have type unit.")
        end;
      Val_unit
      end
  | If_then_else(e1,e2,e3) -> if bool_of_valeur(eval e1 envi) then (eval e2 envi) else (eval e3 envi)

  | Id(s) -> build_fun_value (fun x->x) (List.assoc s envi)

  |Let_id_in(recu, p, e1, e2) ->
    (* recu : rec_state -> indique si c'est la fonction est rec et stocke son id.
       Marche que pour les fonctions !
       p : expr list ((Id _ | Expr_unit) list pour etre plus precis)
       e1, e2 : expr -> ce sont resp. la def de l'id et le bloc dans lequel l'id existe.*)
    begin
      match p with
      | [x] -> eval e2 ((string_of_id x, eval e1 envi)::envi)
             
      | fun_id::para ->
         let envi = (( string_of_id fun_id, build_fun_value (fun _ -> raise (NotAFunction 3)) (eval (Fun(recu, para, e1)) envi) )::envi)
                  (*   ^-----------------^  ^---------------------------------------------------------------------------------^
                       L=> id de la fonciton       L=> construction de sa valeur *)
         in eval e2 envi
        
      | _ -> raise (ToDo "msg err 2")
    end
    
  | Fun(recu, para, op) ->
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
         A mettre dans les tests par la suite.
         Impossible que la fonction soit rec car c'est un lambda.*)
       | Fun(_, para2, op2) -> init_envi_local (id_cat [para; List.map string_of_id para2]) op2 []
       
       | Let_id_in(recu2, x, e1, e2) ->
          begin
            match x with
            (*Ici on definit une nouvelle valeur a un id donc pas besoin de stocker son eventuelle 
              valeur anterieure si l'on tombe sur id apres le "in". En revanche dans la def de sa
              nouvelle valeur, nous en avons besoin.*)
            | [x] -> env_cat [init_envi_local (id_cat [para; fun_id_if_rec recu2]) e1 envi_local;
                              init_envi_local ((string_of_id x)::para) e2 envi_local]

            (*Ici on definit une nouvelle fonction donc pas besoin de stocker l'eventuelle valeur
              anterieure si l'on tombe sur id apres le "in". Les parametres oui car se ne sont plus
              des parametres apres la definition. En revanche dans la def de la fonction, nous en 
              avons besoin mais pas les valeurs des parametres.*)
            | fun_id::para2 ->
               env_cat [init_envi_local ( id_cat [para; List.map string_of_id para2;
                                                  fun_id_if_rec recu2] ) e1 envi_local;
                        init_envi_local (id_cat [para; [string_of_id fun_id]]) e2 envi_local]
                                
            | _ -> raise (ToDo "msg err 3")
          end
         
            
       (*Le reste : appel rec ou cas de base sans faire grandir l'envi_local*)
       | Expr_unit | Const _ | Bool _ | Unary_op _ | Binary_op _ -> envi_local
                           
       | If_then(e1,e2) | App(e1,e2)
         -> env_cat [init_envi_local para e1 envi_local; init_envi_local para e2 envi_local]
         
       | If_then_else(e1,e2,e3) -> env_cat [init_envi_local para e1 envi_local;
         init_envi_local para e2 envi_local; init_envi_local para e3 envi_local]

       | Let_id(_,_,_) -> raise (ToDo "err de syntax")
     in
     begin
       let envi_local = init_envi_local (id_cat [List.map string_of_id para; fun_id_if_rec recu ]) op [] in
       match para with
       | [p0] -> Vf(recu, p0, op, envi_local)
       | p0::_::_ -> Vf(recu, p0, Fun(Non_rec, List.tl para, op), envi_local)
       | [] -> raise (NotAFunction 2)
     end

  | App(f, arg) ->
     begin
     match eval f envi with

     (*fouine fun*)
     | Vf(_, para, op, envi_local) ->
        begin
          match para with
          | Expr_unit -> if (eval arg envi) = Val_unit then eval op envi_local
                         else raise NotExpectedType
                       
          | Id s -> eval op ((s, eval arg envi)::envi_local)

          | _ -> raise (NotAFunction 2)
        end

     (*OCaml fun*)
     | Vf_una g -> g (eval arg envi)

     | Vf_bin g -> Vf_una (g (eval arg envi))
       
     | _ -> raise (NotAFunction 1)
     end

  |Let_id(_,_,_) -> raise (ToDo "Syntax err")


let rec seq_eval seq_e envi_global =
  (*Cette fonction interprete le code fouine. Fait appel eval pour chaque code separes par ";;"
    Entree : seq_e de type expr list et envi_global de type env
    Sortie : env*)
  let special_eval e envi =
    (*Cette fonciton ajoute le cas particulier des Let_id (declaration en surface) a eval. 
      Ces id sont stockes dans l'environnement global.
      Entree : e de type expr et envi de type env
      Sortie : un couple (clef,valeur) de type string*valeur. La clef etant la string de
               l'id si la valeur est associee a un id sinon "-".*)
    match e with
    | Let_id(recu, p, e1) ->
      (* Meme chose que Let_id_in sauf qu'il n'y a pas "in..."*)
      begin
        match p with
        | [x] -> (string_of_id x, eval e1 envi)
             
        | fun_id::para ->
           ( string_of_id fun_id, build_fun_value (fun _ -> raise (NotAFunction 3)) (eval (Fun(recu, para, e1)) envi) )
        (*   ^-----------------^  ^---------------------------------------------------------------------------------^
             L=> id de la fonciton       L=> construction de sa valeur *)
        
        | _ -> raise (ToDo "msg err 2")
      end
      
    | _ -> ("-", eval e envi)
         
  in
  begin
    match seq_e with
    | [e] -> (special_eval e envi_global)::envi_global

    | e::r -> seq_eval r ((special_eval e envi_global)::envi_global)

    | _ -> raise (ToDo "Cas impossible 0")
  end

