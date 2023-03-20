
(* Partie 1 Arbre n-aires :recodage *)

type 'a arbre_n = Feuille of 'a | Noeud of 'a arbre_n list

let a1 = Feuille 1
let a2 = Feuille 2
let a3 = Noeud []
let a4 = Noeud [ a1 ]
let a5 = Noeud [ a1; a2 ]
let a6 = Noeud [ a1; a2; a3; a4; a5 ]

let rec hauteur (l : 'a arbre_n) : int =
  match l with 
  Feuille _ -> 1;
  | Noeud l ->  1 + List.fold_left max 0 (List.map hauteur l) ;;

assert (hauteur a1 = 1);;
assert (hauteur a3 = 1);;
assert (hauteur a4 = 2);;
assert (hauteur a5 = 2);;


let list_of_arbre (a : 'a arbre_n) : 'a list =
  let rec list_of_arbre_aux (a: 'a arbre_n) (acc : 'a list) : 'a list =
    match a with
    Feuille x -> x::acc
    | Noeud l -> List.fold_right list_of_arbre_aux l acc in 
  list_of_arbre_aux a [];;

assert (list_of_arbre a1 = [ 1 ]);;
assert (list_of_arbre a4 = [ 1 ]);;
assert (list_of_arbre a5 = [ 1; 2 ]);;
assert (list_of_arbre a6 = [ 1; 2; 1; 1; 2 ])

(* 1.2 Gestion d'option, fold et minimum *)

let lift_option2 (f : 'a -> 'a -> 'a) : 'a option -> 'a -> 'a option = 
  fun x y -> match x with None -> Some y | Some(z) -> Some(f z y);;


let rec fold_left_arbre (f : 'a -> 'b arbre_n -> 'b arbre_n) (acc : 'b arbre_n) (arbre : 'a arbre_n) : 'b arbre_n = 
  match arbre with
  Feuille x -> f x acc 
  | Noeud l -> 
  