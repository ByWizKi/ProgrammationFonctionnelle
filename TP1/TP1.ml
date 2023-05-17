(*Premiers pas avec l'interperteur*)

(*Definition de fonction*)

(**
    [caculeDiscriminant ax bx c] calcule le discriminant d'un polynome du second degres.
    Cette fonction est utile pour trouve les racines du polynomes ax^2 + bx + c
    @param ax le coef d'ordre 2
    @param bx le coef d'ordre 1
    @param c le coef d'ordre 0
    @return le discriminant
*)
let calculeDiscriminant (ax : float) (bx :float) (c: float) : float = (bx**2.) -. (4.*.ax*.c);;


(*Type Somme*)
type couleur = Rouge 
              | Jaune 
              | Bleu 
              | Violet
              | Orange
              | Vert
              | RJB of int*int*int;;
 
type mes_valeurs = UnInt of int
                  | DeuxInt of int*int
                  | UnString of string;;
let nom_couleur (couleur : couleur) : string =
  match couleur with
  Bleu -> "Bleu"
  |Rouge -> "Rouge"
  |Jaune -> "Jaune"
  |Vert-> "Vert"
  |Orange -> "Orange"
  |_ -> "melange de couleur";;

(**
[paragraphe_bottles est une fonction qui prend en argument un nombre de bouteille et permet de gere l'orthographe des mots pour la chason beer]
@param nombre_bouteille un entier
@return le paragraphe de la chanson avec la bonne orthographe    
*)
let paragraphe_bottles (nombre_bouteille : int) : string =
  match nombre_bouteille with 
  0 ->  "No more bottles of beer on the well, no more bottles of bear\n Go to the store and buy some more, 99 bottles of beer on the wall."
  | 1 -> "1 bottle of beer on the wall, 1 bottle of beer.\n Take one down and pass it around no more bottles of beer on the wall."
  | 2  -> "2 bottles of beer on the wall, 2 bottles of beer. \n Take one doewn and pass it around, 1 bottle of beer in the wall."
  | _ -> string_of_int(nombre_bouteille)^" bottles of beer on the wall, "^string_of_int(nombre_bouteille)^" bottles of beer.\ntake one down and pass it around, "^
  string_of_int(nombre_bouteille-1)^" bottles of beer in the wall.";;

(*Recursivité*)

(**
[factorielle] renvoie la factorielle n
@param n un entier 
@return la factiorielle du paramètre
*)

let rec factiorielle (n:int) : int =
  if(n = 0 || n = 1)
    then 1
else n*factiorielle(n-1);;

(*Listes*)

(**
  [sum_f] est un fonction qui permet de faire la sommes des floats d'une liste
  @param l une liste de floats
  @return un floats qui est la somme des floats de la liste. 
*)

let rec sum_f (l : float list) : float = 
  match l with
  [] -> 0.
  | x::r -> x+.sum_f(r);;

(**
    [join_s] cette fonction permet a partir d'une liste de string et d'un separateur de type string de construire un string
    @param une liste de string
    @param un serapateur de type string
*)
let rec join_s (l : string list) (s : string) = 
  match l with
  [] -> ""
  | [x] -> x^(join_s [] s)
  | x::r -> x^s^(join_s r s);; 

(**
    [liste_n_0] cette fonction permet de fabriquer une liste de n à 0 
    @param n un entier
    @return un liste d'entier dans l'ordre décroissant (n à 0)
*)
let rec liste_n_0 (n:int) : int list = 
  if (n >= 0 )
    then n::(liste_n_0 (n-1))
else [];;

(**
    [bottles_of_liste] cette fonction permet de renvoyer les n paragraphes de la chanson 99 bottles
    @param n un entier qui est le nombre de paragrphe voulue
    @return une liste de string ou les string sont des paragraphes
*)
let rec bottles_of_liste (n : int) : string list= 
  if (n >=0)
    then (paragraphe_bottles n)::bottles_of_liste(n-1)
else [];;

let chanson_99_bottles = join_s (bottles_of_liste 99) "\n\n";;