open Types

exception ToDo of string
exception NotAFunction
exception NotExpectedType

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
  | If_then(e1,e2) -> aff_aux_2e "If_then(" e1 e2
  | If_then_else(e1,e2,e3) -> aff_aux_3e "If_then_else("  e1 e2 e3

  | Id(s) -> print_string ( "Id(" ^ s ^ ")" )
  | Let_id_in(e1,e2,e3) -> aff_aux_3e "Let_id_in(" e1 e2 e3
  | Fun(_,_) -> aff_fun e
  | App(e1,e2) -> aff_aux_2e "App(" e1 e2

  and aff_fun f =
    match f with
    |Fun([],e) -> print_string ("Fun((),"); affiche_expr e; print_string ")"
    |Fun(sl,e) -> 
      begin
        let rec aux l=match l with
          |[] -> affiche_expr e
          |x::r -> ( print_string ("Fun("^x^","); aux r; print_string ")" )
        in aux sl
      end
    |_-> raise NotAFunction
;;

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
  | If_then(e1,e2) ->begin
      if bool_of_valeur(eval e1 envi) then raise (ToDo "eval_unit ou unit dans eval");
      affiche_expr e2; (*pour ne pas avoir le warning tant que c'est WIP*)
      Unit
      end
  | If_then_else(e1,e2,e3) -> if bool_of_valeur(eval e1 envi) then (eval e2 envi) else (eval e3 envi)

  | Id(s) -> snd( List.find (is_id s) envi )

  | Let_id_in(e1,e2,e3) ->
     begin
       match e1 with
       |Id(s) -> (eval e3 ( (s, eval e2 envi)::envi ) )
       |_ -> raise (ToDo "functions etc")
     end
    
  | Fun(p,o) -> Vf(p, o)
     (*begin
       match e1 with
       |Id(s) -> (Id(s), (eval e2))
     end*)

  | App(f,s) ->  (*raise (ToDo "")*)
     let rec eval_envi f envi=
       (* Entree : f de type expr, c'est soit une Fun soit une App.
          Sortie : (g, envi_local) de type expr*env. g est l'evaluation de la fonction f aux 
          premiers parametres qui sont ajoutes a l'environement envi_local. g est donc une Fun.
          envi_local est envi ou on a ajoute les parametres de f dont les valeurs sont connues.
          Parcours f, afin de construire la fonction et son environement pour eval App(f,s)
          ToDo : ameliorer la construction de l'envi (car on met tt alors que pas tt ne sert).*)

       print_string "Debut"; print_newline();
       match f with
       | Fun(_,_) -> f, envi (*Cas de base. La fonciton ne recoit qu'un para.*)

         
              
       | App(g,s) -> (*Cas des fonctions qui recoit deux ou plus parametres. On s'appelle rec, on
                            enleve le premier parametre de la fonction et on le place dans
                            l'environnement local avec sa valeur.*)
          begin
            match eval_envi g envi with
            | Fun(p,o),envi_local ->
               begin
                 match p with (*Structure similaire #1*)
                 | p0::_::_ when default_equal p0 "Unit" ->
                    if bool_of_valeur(eval s envi = Unit)
                    then Fun(List.tl p, o), envi_local
                    else raise NotExpectedType
                   
                 | p0::_::_ -> Fun(List.tl p,o), (p0, eval s envi)::envi_local
                             
                 |_ -> raise (ToDo "Pb pas assez d'argument")
               end
              
            | _,_ -> raise (ToDo "Pb cas impossible")
          end
         
       | _-> raise (ToDo "Pb cas impossible") 

     in
     begin
       match f with
         
       | Fun(p,o) -> (*Cas de base des applications ou le membre de gauche est directement une
                      fonction.*)
          
          begin
            match p with (*Structure similaire #1*)
            (*Si la fonction a un seul parametre, on l'evalue. Sinon ToDo*)
              
            | [p0] when default_equal p0 "Unit" -> if bool_of_valeur(eval s envi = Unit)
                                                   then (eval o envi)
                                                   else raise NotExpectedType
            | [p0] -> (eval o ( (p0, eval s envi)::envi ) )
                    
            | p0::_::_ when default_equal p0 "Unit" -> if bool_of_valeur(eval s envi = Unit)
                                                       then (eval o envi)
                                                       else raise NotExpectedType
            | p0::_::_ -> (eval o ( (p0, eval s envi)::envi ) )
            (*Non ca ne marche pas d'ajouter ainsi le parametre. Il peut etre utilise en dehors
              et ca valeur exterieur est perdue...*)
             
            |_ -> raise (ToDo "Pb ?")
          end
         
       | App(_,_) ->
          begin
            match eval_envi f envi with
            | g,e -> aff_fun g; print_newline(); let _ =List.map (fun (x,y)-> print_string x; print_valeur y; print_newline()) e in eval (App(g,s)) e
          end
         
       |_ -> raise NotAFunction
     end
