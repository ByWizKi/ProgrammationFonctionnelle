
(* Partie 1 Arbre n-aires :recodage *)

type 'a arbre_n = Feuille of 'a | Noeud of 'a arbre_n list;;

let arbre1 = Noeud[Feuille 4; Noeud[Feuille 0; Feuille 1]; Noeud[Feuille 0]];;

(**
    [hauteur_arbre] cette fonction permet de calculer la hauteur d'un arbre avec list.map
    @param un arbre_n
    @return la hauteur d'un arbre
*)
let rec hauteur_arbre (a : 'a arbre_n) : int = 
  match a with
  | Feuille x -> 1 
  | Noeud(f) -> 1 + List.fold_left max  0 (List.map hauteur_arbre f);;

assert(hauteur_arbre arbre1 = 3);;

(**
    [list_of_arbre] cette fonction permet a partir d'un arbre mettre tous les elements dans une liste
    @param un arbre_n
    @return une liste qui contine tout les elements d'un arbre
*)

let list_of_arbre (a : 'a arbre_n) : 'a list = 
  let rec list_of_arbre_aux (a : 'a arbre_n) (acc : 'a list) = 
  match a with 
  Feuille x -> x::acc
  | Noeud(f) -> List.fold_right (list_of_arbre_aux) f acc in 
  list_of_arbre_aux a [];;
  
assert (list_of_arbre arbre1 = [4;0;1;0]);;

(**
    [lift_option_2] ce decorateur permet de d'appliquer une fonction binaire a 2 argument en parametre et de renvoyer le resultat sous type option
    @param une fonction binaires
    @param un resultat et type option
    @param 'a 
    @return 'a option
*)
let lift_option_2 (f : 'a -> 'a -> 'a) : 'a option -> 'a -> 'a option =
    fun x y -> match x with None -> Some y | Some x' -> Some(f y x');;

assert(lift_option_2 min (Some 3) 4  = Some(3));;

(**
    [fold_left_arbre] cette fonction peremet de parcourir un arbre et de lui appliquer une fonction a chacun de ses elements
    @param une fonction qui modifie les elements
    @param un acc
    @param 'a arbre_n
    @return le resultat de la fonction
*)

let rec fold_left_arbre (f : 'b -> 'a -> 'b) (acc : 'b) (a : 'a arbre_n) : 'b = 
  match a with 
  Feuille x -> f acc x
  | Noeud n -> List.fold_left (fold_left_arbre f) acc n;;

(**
    [reduce] cette fonction permet de combiner 2 resultat de maniere generaliser avec la fonction fold_left_arbre
    @param f une fonction
    @param un 'a arbre_n
*)

let reduce (f : 'b -> 'a -> 'b) (a : 'a arbre_n) : 'b option = 
  (fold_left_arbre (lift_option_2 f) None a);;

assert(reduce (min) arbre1 = Some 0);;

