(*Arbre n-aires*)

type 'a arbre_n = Feuille of 'a | Noeud of 'a arbre_n list;; 

let arbre1 = Feuille 3;;

let arbre2 = Noeud[Feuille 3; Noeud [Feuille 0; Feuille 1]; Noeud[Feuille 2; Feuille 5; Noeud[Feuille 6]]];;

let arbreVide = Noeud[];;

(**
    [hauteurForet] cette fonction permet de calculer la hauteur d'un arbre
    @param un arbre_n
    @return la hateur de l'arbre sous forme de int
*)

let rec hauteur_arbre (a : 'a arbre_n) : int = 
  match a with 
  | Feuille _ -> 1
  | Noeud(f) -> 1 + hauteur_foret f
and  hauteur_foret (a : 'a arbre_n list) : int = 
  match a with
  [] -> 0
  | x::r -> max (hauteur_arbre x)  (hauteur_foret r);;

(**
    [list_of_arbre_aux / ] cette fonction permet de mettre tous les élements d'un arbre dans une liste 
    @param un arbre
    @return une liste qui contient les élement de l'arbre
*)
let list_of_arbre (a: 'a arbre_n) : 'a list= 
let rec list_of_arbre_aux (a : 'a arbre_n) (acc : 'a list) = 
  match a with
  Feuille x -> x::acc
  | Noeud(f) -> list_of_foret f acc
  and list_of_foret (a : 'a arbre_n list) (acc : 'a list) = 
  match a with
  [] -> acc
  |x::r -> list_of_foret r (list_of_arbre_aux x acc) in 
list_of_arbre_aux a [];;

assert(list_of_arbre arbre2 = [6; 5; 2; 1; 0; 3]);;

(**
    [minimum] cette fonction permet de determiner la valeur minimal d'un arbre
    @param un arbre_n
    @renvoi un 'a option
*)

let minimum (a: 'a arbre_n) : 'a option = 
  let rec minimum_aux (a: 'a arbre_n) (acc : 'a option) : 'a option =
    match a with
    | Feuille x  -> (
      match acc with
      | None -> Some x
      | Some y -> Some (min x y))
    | Noeud(f) -> minimum_foret f acc 
    and minimum_foret (a : 'a arbre_n list) (acc : 'a option) = 
    match a with
    [] -> acc
    | x::r -> minimum_foret r (minimum_aux x acc) in 
  minimum_aux a None;;

assert(minimum arbre2 = Some 0);;

(**
    [reduce] cette fonction permet de combiner 2 resultats de maniere generaliser
    @param une fonction qui compare 2 élements ('a -> 'a -> 'a) 
    @param un arbre_n
    @return 'a option
*)

let reduce (f : 'a -> 'a -> 'a) (a : 'a arbre_n) : 'a option= 
  let rec reduce_aux (f : 'a -> 'a -> 'a) (a : 'a arbre_n) (acc : 'a option) : 'a option =
    match a with
    | Feuille x  -> (
      match acc with
      | None -> Some x
      | Some y -> Some (f x y))
    | Noeud(n) -> reduce_foret n acc 
    and reduce_foret (a : 'a arbre_n list) (acc : 'a option) = 
    match a with
    [] -> acc
    | x::r -> reduce_foret r (reduce_aux f x acc) in 
  reduce_aux f a None;;

assert(reduce (+) arbre2 = Some 17);;
assert(reduce min arbre2 = Some 0);;

(*FIFO Basé sur des listes*)

type 'a fifo = L of 'a list * 'a list;;

let fifo1  = L([3;4;4], [1;2;3;4]);;
let fifo2 = L([3;4;5;6;7], [1;2;5;6]);;
let fifo3 = L([1;2;3;4], []);;

(**
    [push_fifo] cette fonction permet de 'dajouter un element a une fifo
    @param une fifo 'a 
    @param un element a ajouter 'a 
    @return une fifo 'a avec un element en plus
*)

let push_fifo (fifo : 'a fifo) (e : 'a) : 'a fifo = 
  match fifo with
  L(x, y) -> L(e::x, y);;
assert(push_fifo fifo1 4 = L([4; 3; 4; 4], [1; 2; 3; 4]));;

(**
    [push_list_fifo] cette fonction permet de construire une fifo a partir d'une fifo existante et d'une liste
    @param une 'a fifo
    @param une 'a list
    @return un 'a fifo
*)

let rec push_list (fifo : 'a fifo) (l : 'a list) : 'a fifo = 
  match l with
  [] -> fifo
  | x::r -> push_fifo (push_list fifo r) x ;;

assert(push_list fifo1 [1;2;3;4] = L ([1; 2; 3; 4; 3; 4; 4], [1; 2; 3; 4]));;

(**
    [transfert_fifo] cette fonction permet de déplacer les elements de la liste de gauche dans celle de la liste de droite en inversant leur ordre
    @param 'a fifo
    @return a fifo avec le transfert effectuer
*)

let rec transfert_fifo (fifo : 'a fifo) : 'a fifo = 
  match fifo with
  L(lg, ld) -> (
    match lg with
    [] -> L([], ld)
    | x::r -> transfert_fifo (L(r, x::ld))
  );;

assert (transfert_fifo fifo3 = L ([], [4; 3; 2; 1]));;

(**
    [pop_fifo] cette fonction permet de recuperer un element dans une fifo
    @param 'a fifo
    @return un couple avec la fifo restante et l'element extrait sous forme de 'a option
*)

let rec pop_fifo (fifo : 'a fifo) : ('a fifo * 'a option) = 
  match fifo with
  L([], []) -> (L([], []), None)
  | L(lg, ld) -> (
    match ld with 
    [] -> pop_fifo (transfert_fifo fifo)
    | x::r -> (L(lg, r), Some x))
  ;;

assert(pop_fifo fifo2 = (L ([3; 4; 5; 6; 7], [2; 5; 6]), Some 1));;

(**
    [pop_all_fifo] cette fonction permet d'enlver tous les elements d'une fifo et de les ranger dans l'ordre dans une liste
    @param une fifo
    @return 'a list 
*)

let rec pop_all_fifo (fifo : 'a fifo) : 'a list = 
  match fifo with
  L([], []) -> []
  | L(lg, ld) -> (
    match ld with
    [] -> pop_all_fifo (transfert_fifo fifo)
    | x::r -> x::pop_all_fifo (L(lg, r))
  );;

assert(pop_all_fifo fifo2 = [1; 2; 5; 6; 7; 6; 5; 4; 3]);;

