(*Arbre n-aires*)

type 'a arbre_n = Feuille of 'a | Noeud of 'a arbre_n list;; 

let arbre1 = Feuille 3;;

let arbre2 = Noeud[Feuille 3; Noeud [Feuille 0; Feuille 1]];;

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
    [list]*)