(*Premiers pas avec l'interperteur*)

(*Definition de fonction*)
(* fonction qui ajoute 1 *)
let f (x:int) :int = x+1;;

(*fonction qui calcule le discriminant*)
let discriminant (a:int)  (b:int)  (c:int) :float = 
  float_of_int(b*b-(4*a*c));;

(*Type somme*)

type couleur = Rouge
              | Jaune
              | Bleu
              | Violet
              | Orange
              | Vert
              | RJB of int * int * int ;;

type mes_valeur = UnInt of int
                  | DeuxInt of int * int
                  | UneChaine of string;;

(*Pattern matching*)

let nom_couleur (c:couleur) : string =
  match c with
  Bleu -> "bleu"
  | Rouge -> "rouge" 
  | Jaune -> "jaune"
  | Violet -> "violet"
  | Orange -> "orange"
  | Vert -> "vert"
  | RJB(_,_,_) -> "melange";;


let niveau_rouge c =
  match c with
  | Rouge -> 255
  | Orange -> 255
  | Violet -> 255
  | RJB (r,j,b) -> r
  | _ ->  0;;

let paragraphe_bottles (nb:int) : string = 
  match nb with
  0 -> "No more bottles of beer on the wall, no more bottles of beer.
  Go to the store and buy some more, 99 bottles of beer on the wall."
  | 1 -> "1 bottle of beer on the wall, 1 bottle of beer. Take one down and pass it around, no more bottles of beer on the wall."
  | 2 -> "2 bottles of beer on the wall, 2 bottles of beer. Take one down and pass it around, 1 bottle of beer on the wall."
  | x -> string_of_int x ^ " bottles of beer on the wall, " ^ string_of_int x ^ " bottles of beer. Take one down and pass it around, " ^ string_of_int (x-1) ^ " bottles of beer on the wall.";;

(*Recursivite*)

let rec factorielle (n:int) : int =
  match n with 
  0 -> 1
  | 1 -> 1
  | _ -> n * factorielle(n-1);;

(*Liste*)

let rec join_s (s:string) (l: string list) : string =
  match l with
   [] -> ""
  | x::r -> x ^ s ^ join_s s r;; 

let rec liste_n_0 (n:int) : int list =
  match n with
  0 -> [0]
  | _ -> n::liste_n_0 (n-1);;

let rec bottles_of_list (l:int list) = 
  match l with 
  [] -> failwith("Aucun int dans la liste")
  | [x] -> [paragraphe_bottles x]
  | x::r -> paragraphe_bottles x :: bottles_of_list r;;

(* let chanson_99_bottles = join_s "\n" ((bottles_of_list) (liste_n_0 99));;
print_endline chanson_99_bottles;; *)