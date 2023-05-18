(*Arbres binaires*)

type arbre_bin = ABVide | ABNoeud of int * arbre_bin * arbre_bin;;

let ab1 = ABNoeud (3, ABVide, ABNoeud(4, ABVide, ABVide));;
let ab2 = ABVide;;

(**
    [tailleAbrebin] cette fonction permet de calculer le nombre de noued qui sont stocker dans un arbre
    @param un arbre binaire
    @return un entier (la taille de l'arbre)
*)

let rec taille_arbre  (a : arbre_bin) : int =
  match a with
  ABVide -> 0
  | ABNoeud(v, fg, fd) -> 1 + taille_arbre fg + taille_arbre fd;;

assert(taille_arbre ABVide = 0);;
assert(taille_arbre ab1 = 2);;


(**
    [produit_arbre] cette fonction permet de faire le produit de chaque noeud d'un arbre
    @param un abre binaire
    @return le produit de chaque noeud (un int) 
*)
let rec produit_arbre (a : arbre_bin) : int = 
  match a with 
  ABVide -> 1
  | ABNoeud(x, fg, fd) -> x * (produit_arbre fg) * (produit_arbre fd);;

assert(produit_arbre ab1 = 12);;
assert(produit_arbre ABVide = 1);;

(**
    [insere_arbre_bin_recherche] cette fonction permet de d'insert un element dans un arbre binaire de recherche
    @param e un int qui est l'elememt a inserer
    @param a un arbre binaire de recherche ou l'on souhaite inserer notre nouveau élement
    @return un arbre binaire de recherche à jour
*)

let rec insere_arbre_bin_recherche (e : int) (a : arbre_bin) : arbre_bin = 
  match a with
  ABVide -> ABNoeud(e, ABVide, ABVide)
  | ABNoeud(x, fg, fd) -> if e < x
                            then ABNoeud(x, insere_arbre_bin_recherche e fg, fd)
                          else ABNoeud(x, fg, insere_arbre_bin_recherche e fd);;
                        
assert(insere_arbre_bin_recherche 3 ab1 = ABNoeud (3, ABVide, ABNoeud (4, ABNoeud (3, ABVide, ABVide), ABVide)));;

(**
    [list_of_arbre_bin] cette fonction permet de parcourir la un arbre_bin sous le parcours infixe les elements sont stocker dans une liste
    @param un arbre binaire
    @return un liste de int dans l'ordre infixe
*)

let rec list_of_arbre_bin (a : arbre_bin) : int list =
  match a with
  ABVide -> []
  | ABNoeud(x, fg, fd) -> list_of_arbre_bin fg @ [x] @ list_of_arbre_bin fd;;

assert(list_of_arbre_bin (ABNoeud(3, ABNoeud(2, ABNoeud(1, ABVide, ABVide), ABVide), ABNoeud(4, ABVide, ABVide))) = [1; 2; 3; 4]);;

(**
    [arbre_bin_rech_of_int_list] cette fonction permet de transfomer une liste en arbre binaire de recherche
    @param une liste qui contient des int 
    @return un arbre binaire de recherche
*)

let rec arbre_bin_rech_of_int_list (l : int list)  : arbre_bin =
  match l with
  [] -> ABVide
  | x::r -> insere_arbre_bin_recherche x (arbre_bin_rech_of_int_list r);;

assert (arbre_bin_rech_of_int_list [1;2;3] = ABNoeud (3, ABNoeud (2, ABNoeud (1, ABVide, ABVide), ABVide), ABVide));;

(**
    [tri_abr] cette fonction permet de trier une liste d'entier
    @param une liste d'entier 
    @return une liste trier
*)

let tri_abr (l : int list) : int list =
  list_of_arbre_bin(arbre_bin_rech_of_int_list(l));;

assert(tri_abr [3;2;5;6;2] = [2;2;3;5;6]);;

(*Evaluation d'expression arithmétiques*)

type binop = Plus | Moins | Mult | Div;;

type expr = Cst of int | Binop of binop * expr * expr;;

type eval_err = DivZero;;

type resultat = Ok of int | Err of eval_err;;
let expr1 = Binop (Plus, Cst 3, Cst 4);;
let expr2 = Binop(Plus, Binop(Mult, Cst 5, Cst 5), Binop(Moins, Cst 100, Cst 100));;

(**
    [string_of_epxr] cette fonction permet de transformer une expression en une chaine de caractère
    @param une expréssion
    @return une string qui traduit l'expression
*)

let rec string_of_expr (e : expr) : string =
  match e with
  Cst(x) -> string_of_int x ^ ""
  | Binop(op, u, v) -> if op = Plus
                          then "("^string_of_expr u ^ "+" ^ string_of_expr v^")"
                        else if op = Moins
                                then "("^string_of_expr u ^ "-" ^ string_of_expr v^")"
                              else if op = Mult 
                                    then "("^string_of_expr u ^ "*" ^ string_of_expr v^")"
                                  else "("^string_of_expr u ^ "/" ^ string_of_expr v^")";;
                      
assert(string_of_expr expr2 = "((5*5)+(100-100))");;

(**
    [eval_expr] cette fonction permet dévaluer une expression
    @param une expression a evaluer
    @return le resultat de l'expression a evaluer sous la forme d'un type resultat
*)

let rec eval_expr (e : expr) : resultat =
  match e with
  Cst(x) -> Ok(x)
  | Binop(op, u, v) -> (
    match (eval_expr u, eval_expr v) with
    | Ok(x), Ok(y) -> (
      match op with
      | Plus -> Ok(x+y)
      | Mult -> Ok(x*y)
      | Moins -> Ok(x-y)
      | Div -> if y = 0 then Err(DivZero) else Ok(x/y))
    |(_, _) -> failwith("Erreur"));;


assert (eval_expr expr1 = Ok(7));;
assert (eval_expr expr2 = Ok(25));;