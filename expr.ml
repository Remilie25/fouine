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
       | [s] -> aff_aux "Let_id_in(" [Id s;e1;e2]
       | fun_id::para -> (print_string "Let_id_in("; affiche_expr (Id fun_id); print_string ", ";
                          aff_fun (Fun(para, e1));
                          aff_aux ", " [e2])
       | _ -> raise (ToDo "msg err")
     end
    
  | Fun(_,_) -> aff_fun e
  | App(e1,e2) -> aff_aux "App(" [e1;e2]

  and aff_fun f =
    match f with
    (*|Fun([],e) -> print_string ("Fun((),"); affiche_expr e; print_string ")" Cas inutile*)
    | Fun(sl,e) -> 
      begin
        let rec aux l=match l with
          |[] -> affiche_expr e
          |x::r -> ( print_string ("Fun("^x^", "); aux r; print_string ")" )
        in aux sl
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
  |(s,_) -> default_equal s v;;

let rec env_cat env_list =
  (*Cette fonciton concatene tous les environnements sans doublons.
    Les env sont supposes sans doublons*)
  
  let rec aux env1 env2 = match env2 with
    (*Parcourt l'env2 en ajoutant les id dans env1 sans doublons.*)
    | [] -> env1
    | (s, _)::r when List.exists (fun couple -> s = (fst couple)) env1 -> aux env1 r
    | (s, v)::r -> (s, v) :: (aux env1 r)
  in
  match env_list with
  | [env] -> env
  | [] -> []
  | env::_::_ -> aux (env_cat (List.tl env_list)) env;; 


                      
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
      Unit
      end
  | If_then_else(e1,e2,e3) -> if bool_of_valeur(eval e1 envi) then (eval e2 envi) else (eval e3 envi)

  | Id(s) -> if s = "Unit" then Unit
             else snd( List.find (is_id s) envi )
               (* pas de distinction selon le type ? WIP
     match snd( List.find (is_id s) envi ) with
             |Vc(k) -> Vc(k)
             |Vb(b) -> Vb(b)
             |Vf(para, op, arg) -> Vf(para, op, arg)*)
           
  |Let_id_in(p, e1, e2) ->
    begin
      match p with
      | [s] -> eval e2 ((s, eval e1 envi)::envi)
      | fun_id::para -> eval e2 (( fun_id, eval (Fun(para, e1)) envi )::envi)
      | _ -> raise (ToDo "msg err")
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
       | Id(s) when (List.mem s para || s = "Unit") -> envi_local

       | Id(s) -> ( s, eval (Id(s)) envi )::envi_local

                
       (*Pour garder les bonnes valuers. Ex ou il pourraient y avoir des pb sans :
         let k = 1 in let fun_test x= let aux l= k+l in aux x in let k = 2 in fun_test k
         A mettre dans les tests par la suite.*)
       | Fun(para2, op2) -> init_envi_local (para @ para2) op2 []
       
       | Let_id_in(x,e1,e2) ->
          begin
            match x with
            (*Ici on definit une nouvelle valeur a un id donc pas besoin de stocker son eventuelle 
              valeur anterieure si l'on tombe sur id apres le "in". En revanche dans la def de sa
              nouvelle valeur, nous en avons besoin.*)
            | [s] -> env_cat [init_envi_local para e1 envi_local; init_envi_local (s::para) e2 envi_local]

            (*Ici on definit une nouvelle fonction donc pas besoin de stocker l'eventuelle valeur
              anterieure si l'on tombe sur id apres le "in". Les parametres oui car se ne sont plus des
              parametres apres la definition. En revanche dans la def de la fonction, nous en avons
              besoin mais pas les valeurs des parametres.*)
            | fun_id::para ->
               env_cat [init_envi_local para e1 envi_local; init_envi_local [fun_id] e2 envi_local]
                                
            | _ -> raise (ToDo "msg err")
          end
         
            
       (*Le reste : appel rec ou cas de base sans faire grandir l'envi_local*)
       | Const _ | Bool _ -> envi_local

       | Not(e1) -> init_envi_local para e1 envi_local
                           
       | Add(e1,e2) | Mul(e1,e2) | Min(e1,e2) | Div(e1,e2) | Eq(e1,e2) | Grt(e1,e2) | Or(e1,e2)
         | And(e1,e2) | If_then(e1,e2) | App(e1,e2)
         -> env_cat [init_envi_local para e1 envi_local; init_envi_local para e2 envi_local]
         
       | If_then_else(e1,e2,e3) -> env_cat [init_envi_local para e1 envi_local;
         init_envi_local para e2 envi_local; init_envi_local para e3 envi_local]
     in

     Vf(para, op, init_envi_local para op []) (*Il faudrait evaluer un envi_local ici avec var libre*)

  | App(f, arg) ->
     let rec eval_envi f envi=
       (* Entree : f de type expr, c'est soit une Fun soit une App.
          Sortie : (g, envi_local) de type expr*env. g est l'evaluation de la fonction f aux 
          premiers parametres qui sont ajoutes a l'environement envi_local. g est donc une Fun.
          envi_local est envi ou on a ajoute les parametres de f dont les valeurs sont connues.
          Parcours f, afin de construire la fonction et son environement pour eval App(f,s)
          ToDo : ameliorer la construction de l'envi (car on met tt alors que pas tt ne sert).*)

       match f with
       (*Cas de base. La fonciton ne recoit qu'un para.*)
       | Fun(_,_) -> f, envi

       (*Cas de base ou la fonciton est stockee dans un id.*)
       | Id(s) ->
          begin
              match snd( List.find (is_id s) envi ) with
              | Vf(para,op,envi_local) -> (Fun(para,op)), envi_local
                                        
              | _ -> raise (NotAFunction 2)
         end

         
              
       | App(g,arg) -> (*Cas des fonctions qui recoit deux ou plus parametres. On s'appelle rec.*)
          begin
            match eval_envi g envi with

            (*On enleve le premier parametre de la fonction et on le place dans l'environnement 
              local avec sa valeur.*)
            | Fun(para,op),envi_local ->
               begin
                 match para with (*Structure similaire #1*)
                 | p0::_::_ when p0 = "Unit" ->
                    if eval arg envi = Unit then Fun(List.tl para, op), envi_local
                    else raise NotExpectedType
                   
                 | p0::_::_ -> Fun(List.tl para, op), (p0, eval arg envi)::envi_local
                            
                 |_ -> raise (ToDo "Pb pas assez d'argument")
               end

           
            (*L'eval_envi renvoie une App => le seul cas possible est de venir d'une id. Rien a faire.*)
            (*| App(h,v),e -> App(h,v),e*)
              
            | h,_ -> raise (affiche_expr h ;(ToDo "Pb cas impossible 2"))
          end
         
       | _-> raise (ToDo "Pb cas impossible 1")

     in
     begin
       match f with
         
       | Fun(para,op) -> (*Cas de base des applications ou le membre de gauche est directement une
                      fonction.*)

          begin
            match para with (*Structure similaire #1*)
            (*Si la fonction a un seul parametre, on l'evalue. Sinon ToDo*)
              
            | [p0] when p0 = "Unit" -> if eval arg envi = Unit then (eval op envi)
                                       else raise NotExpectedType
            | [p0] -> ( eval op ( (p0, eval arg envi)::envi ) )
                    
            | p0::_::_ when p0 = "Unit" -> if eval arg envi = Unit then (eval (Fun(List.tl para, op)) envi)
                                           else raise NotExpectedType
            | p0::_::_ -> (eval (Fun(List.tl para, op)) ( (p0, eval arg envi)::envi ) )
            (*Non ca ne marche pas d'ajouter ainsi le parametre. Il peut etre utilise en dehors
              et ca valeur exterieur est perdue...*)
             
            |_ -> raise (ToDo "Pb ?")
          end
         
       | App(_,_) ->
          begin
            match eval_envi f envi with
            | g,e -> (*(* Debug:*) print_string "g = " ; aff_fun g; print_newline(); let _ =List.map (fun (x,y)-> print_string (x^" = "); print_valeur y; print_newline()) e in*) eval (App(g,arg)) e
          end

       | Id(s) ->
          begin
            match snd( List.find (is_id s) envi ) with
            | Vf(para,op,envi_local) -> eval (App(Fun(para,op), arg)) envi_local
                                      
            | _ -> raise (NotAFunction 2)
          end
       |_ -> raise (NotAFunction 1)
     end
  
