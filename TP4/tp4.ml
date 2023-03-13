(**********************************************************************)
(* Arbres n-aires *)
(**********************************************************************)
(** Arbre avec un nombre quelconque de fils *)
type 'a arbre_n = Feuille of 'a | Noeud of 'a arbre_n list

let a1 = Feuille 1
let a2 = Feuille 2
let a3 = Noeud []
let a4 = Noeud [ a1 ]
let a5 = Noeud [ a1; a2 ]
let a6 = Noeud [ a1; a2; a3; a4; a5 ]
let a_vide_1 = Noeud []
let a_vide_2 = Noeud [ Noeud [] ]
(* Le type de ces arbres vide est 'a arbre_n. En effet, comme ces arbres ne
   contiennent pas d'éléments ils peuvent être vus comme des arbresavec ce qu'on
   veut comme type d'élément. *)

let rec hauteur (a : 'a arbre_n) : int =
  match a with Feuille _ -> 1 | Noeud l -> hauteur_foret l + 1

and hauteur_foret (l : 'arbre_n list) : int =
  match l with
  | [] -> 0
  | a :: l' -> max (hauteur a) (hauteur_foret l')
;;

assert (hauteur a1 = 1);;
assert (hauteur a3 = 1);;
assert (hauteur a4 = 2);;
assert (hauteur a5 = 2);;
assert (hauteur a6 = 3);;

(**
Renvoie une liste contenant tous les éléments de l'arbre

@param a: l'arbre
@return la liste de ses éléments
*)
let list_of_arbre (a : 'a arbre_n) : 'a list =
  let rec list_of_arbre_aux (a : 'a arbre_n) (acc : 'a list) : 'a list
      =
    match a with
    | Feuille x -> x :: acc
    | Noeud f -> list_of_foret f acc
  and list_of_foret (f : 'a arbre_n list) (acc : 'a list) : 'a list =
    match f with
    | [] -> acc
    | a :: f' -> list_of_arbre_aux a (list_of_foret f' acc)
  in
  list_of_arbre_aux a []
;;

assert (list_of_arbre a1 = [ 1 ]);;
assert (list_of_arbre a4 = [ 1 ]);;
assert (list_of_arbre a5 = [ 1; 2 ]);;
assert (list_of_arbre a6 = [ 1; 2; 1; 1; 2 ])

(**
[minimum arbre] est le plus grand élément de arbre si arbre en contient au moins 1.

@param arbre l'arbre dans lequel on cherche le minimum
@return None si l'arbre ne contient pas d'élément, ou sinon Some m avec m le plus grand élément de l'arbre 
*)
let rec minimum (arbre : 'a arbre_n) : 'a option =
  match arbre with
  | Feuille x -> Some x
  | Noeud la -> minimum_foret la

(**
[minimum_foret l] donne l'élément minimal que l'on peut trouver dans une forêt

@param l la forêt
@return None si la forêt ne contient pas d'élément ou sinon Some m où m est le plus grand élément de la forêt
*)
and minimum_foret (la : 'a arbre_n list) : 'a option =
  match la with
  | [] -> None
  | a :: la' -> (
      match minimum_foret la' with
      | None -> minimum a
      | Some n -> (
          match minimum a with
          | None -> Some n
          | Some n' -> Some (min n n')))
;;

assert (minimum a1 = Some 1);;
assert (minimum a3 = None);;
assert (minimum a4 = Some 1);;
assert (minimum a5 = Some 1);;
assert (minimum a6 = Some 1)

(**
[reduce f a] renvoie:
- None si a ne contient aucun élément
- Some x si a contient un seul élément x
- Some x où x est le résultat de la combinaison des éléments de a en utilisant f

@param f la fonction de combinaison des éléments
@param a l'arbre qui contient les éléments
*)
let rec reduce (f : 'a -> 'a -> 'a) (arbre : 'a arbre_n) : 'a option =
  match arbre with Feuille x -> Some x | Noeud l -> reduce_foret f l

(**
[reduce_foret f l] renvoie:
- None si l (en tant que forêt) ne contient aucun élément
- Some x si l contient un seul élément x
- Some x où x est le résultat de la combinaison des éléments de l en utilisant f

@param f la fonction de combinaison des éléments
@param la forêt qui contient les éléments
*)
and reduce_foret (f : 'a -> 'a -> 'a) (la : 'a arbre_n list) :
    'a option =
  match la with
  | [] -> None
  | a :: la' -> (
      match reduce_foret f la' with
      | None -> reduce f a
      | Some n -> (
          match reduce f a with
          | None -> Some n
          | Some n' -> Some (f n n')))
;;

assert (reduce min a1 = Some 1);;
assert (reduce min a3 = None);;
assert (reduce min a4 = Some 1);;
assert (reduce min a5 = Some 1);;
assert (reduce min a6 = Some 1);;
assert (reduce ( + ) a1 = Some 1);;
assert (reduce ( + ) a3 = None);;
assert (reduce ( + ) a5 = Some 3);;
assert (reduce ( + ) a6 = Some 7)

(**********************************************************************)
(* Files (FIFO) implémentées avec deux listes *)
(**********************************************************************)

type 'a fifo = Fifo of ('a list * 'a list)

(** File vide *)
let empty_fifo : 'a fifo = Fifo ([], [])

(**
[push_fifo e f] Ajoute e dans f

@param e l'élément a ajouter
@param f la fifo dans laquelle on veut ajouter l'élément
@return la fifo contenant les éléments de f puis e
*)
let push_fifo (e : 'a) (f : 'a fifo) : 'a fifo =
  match f with Fifo (l1, l2) -> Fifo (e :: l1, l2)

let f1 = push_fifo 1 empty_fifo
let f2 = push_fifo 2 f1
let f3 = push_fifo 3 f2
let f4 = push_fifo 4 f3;;

assert (f1 = Fifo ([ 1 ], []));;
assert (f2 = Fifo ([ 2; 1 ], []));;
assert (f3 = Fifo ([ 3; 2; 1 ], []));;
assert (f4 = Fifo ([ 4; 3; 2; 1 ], []))

(**
[push_list_fifo l f] ajoute les éléments de l à la file f

@param l les éléments à ajouter
@param f la file dans laquelle ajouter les éléments
@return la file contenant les éléments de f puis les éléments de l
*)
let rec push_list_fifo (l : 'a list) (f : 'a fifo) : 'a fifo =
  match l with
  | [] -> f
  | x :: l' -> push_list_fifo l' (push_fifo x f)
;;

assert (push_list_fifo [] empty_fifo = empty_fifo);;
assert (push_list_fifo [] f2 = f2);;
assert (push_list_fifo [ 1 ] empty_fifo = f1);;
assert (push_list_fifo [ 3; 4 ] f2 = f4);;
assert (push_list_fifo [ 1; 2; 3; 4 ] empty_fifo = f4)

(**
Fonction utilitaire transférant tous les éléments de la liste de gauche dans
celle de droite en en renversant l'ordre au passage.
*)
let rec transfert_fifo (f : 'a fifo) : 'a fifo =
  match f with
  | Fifo ([], l2) -> Fifo ([], l2)
  | Fifo (x :: l1, l2) -> transfert_fifo (Fifo (l1, x :: l2))
;;

assert (transfert_fifo f4 = Fifo ([], [ 1; 2; 3; 4 ]));;
assert (transfert_fifo f1 = Fifo ([], [ 1 ]))

(**
[pop_fifo f] renvoie le premier élément de f s'il y en a un, ainsi que la file contenant le reste des éléments de f.

@param f la file dans laquelle on veut prendre un élément
@return (f',r) où
- f' est la file contenant les éléments de f sauf le premier
- r est Some x si f a pour premier élément x ou bien None si f est vide
*)
let pop_fifo (f : 'a fifo) : 'a fifo * 'a option =
  match f with
  | Fifo (l1, []) -> (
      match transfert_fifo f with
      | Fifo (_, []) -> (Fifo ([], []), None)
      | Fifo (_, x :: l2') -> (Fifo ([], l2'), Some x))
  | Fifo (l1, x :: l2') -> (Fifo (l1, l2'), Some x)
;;

assert (pop_fifo empty_fifo = (empty_fifo, None));;
assert (pop_fifo f1 = (empty_fifo, Some 1));;
assert (pop_fifo f2 = (Fifo ([], [ 2 ]), Some 1));;
assert (pop_fifo (fst (pop_fifo f2)) = (empty_fifo, Some 2))

(**
Renvoie tous les éléments de la file dans l'ordre de celle-ci

@param f la file dont on veut les éléments
@return une liste contenant les éléments de f dans l'ordre
*)
let rec pop_all_fifo (f : 'a fifo) : 'a list =
  match pop_fifo f with
  | _, None -> []
  | f', Some x -> x :: pop_all_fifo f'
;;

assert (pop_all_fifo empty_fifo = []);;
assert (pop_all_fifo f1 = [ 1 ]);;
assert (pop_all_fifo f4 = [ 1; 2; 3; 4 ]);;

(* Un test mélangeant les opérations de push et de pop de la file *)
assert (
  pop_all_fifo (push_list_fifo [ 3; 4 ] (fst (pop_fifo f2)))
  = [ 2; 3; 4 ])
