type arbre_bin = ABVide (* regle a 0 valeur *)
                | ABNoeud of int * arbre_bin * arbre_bin(*regle a 2 valeurs*);;

let rec taille_ab (a: arbre_bin): int = 
  match  a with
  ABVide -> 0
  | ABNoeud (_, fg, fd) -> (taille_ab fg) + (taille_ab fd) + 1;; 


let rec produit_ab (a: arbre_bin) : int = 
  match a with
  ABVide -> 1
  | ABNoeud (x, fg, fd) -> x * (produit_ab fg) * (produit_ab fd) ;;

let rec insere_arbre_bin_recherche (a : arbre_bin) (e: int) :arbre_bin = 
  match a with
  ABVide -> ABNoeud(e, ABVide, ABVide)
  | ABNoeud(x, fg, fd) -> if x > e
                            then ABNoeud(x, insere_arbre_bin_recherche fg e, fd)
                          else ABNoeud(x, fg, insere_arbre_bin_recherche fd e);;

let rec list_of_arbre_bin (a : arbre_bin) (l : int list) : int list = 
  match a with 
  ABVide -> []
  | ABNoeud(x, fg, fd) -> (list_of_arbre_bin fg l) @ (x :: list_of_arbre_bin fd l);;


let rec arbre_bin_rech_of_int_list (a: arbre_bin) (l: int list) : arbre_bin = 
  match l with 
  [] -> ABVide
  | x::r -> arbre_bin_rech_of_int_list (insere_arbre_bin_recherche a x) r;;


let tri_abr (arbre : arbre_bin) (l : int list) : int list = 
  list_of_arbre_bin (arbre_bin_rech_of_int_list ABVide (list_of_arbre_bin arbre [])) [];;

(*Evalutation d'expression arithmetiques*) 

type binop = Plus | Moins | Mult | Div;;

type expr = Cst of int | Binop of binop * expr * expr;;

type eval_err = DivZero;;

type resultat = Ok of int |Err of eval_err ;;


(* 
let rec eval_expr (expression : expr) : resultat = 
  match expression with
  Cst x -> Ok x  
  | Binop(op, Cst x, Cst y) -> (match op with  
                              Plus -> Ok (x+y) 
                              | Moins -> Ok (x-y) 
                              | Mult -> Ok (x*y)
                              | Div -> if x = 0 || y=0
                                        then Err DivZero
                                      else Ok(x/y))

  | Binop(op, op1, op2) -> eval_expr (Binop(op, (eval_expr_aux op1), (eval_expr_aux op2)));; *)


