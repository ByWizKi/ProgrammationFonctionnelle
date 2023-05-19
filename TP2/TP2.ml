(*Concatenations de listes*)

(**
		[concatene] cette fonction permet de concatener 2 listes ensemble*
		@param l1 qui va etre la premiere liste
		@param l2 qui va etre la deuxieme liste
		@return un liste qui possede les elements de la lsite l1 puis de la liste l2
*)
let rec concatene (l1 : int list) (l2 : int list) :int list = 
	match l1 with
	[] -> l2
	| x::r -> x::concatene r l2;;
(**
		[applatit] cette fonction permet de faire de réduire une int list list en un int list
		@param l la liste a réduire
		@return renvoie une int liste*)
let rec applatit (l : int list list) : int list = 
	match l with
	[] -> []
	| [x] -> x
	| x::y::r -> applatit ((concatene x y)::r);;

(*Retournment de liste*)

(**
		[renverse_ajoute] cette fonction permet de faire la concatenation entre 2 liste mais la liste 1 est rensever dans la liste renvoyer
		@param l1 int list
		@param l2 int list
		@return renvoie une liste*)
let rec renverse_ajoute (l1 : int list) (l2 : int list) : int list = 
	match l1 with
	[] -> l2
	| x::r -> renverse_ajoute r (x::l2);;
(**
		[renverse] cette fonction permet de renverser une liste
		@param l une int liste 
		@return la liste renverser*)
let renverse (l : int list) : int list = 
	let rec renverse_ajoute (l1 : int list) (l2 :int list) : int list = 
		match l1 with 
		[] -> l2
		|x::r-> renverse_ajoute r (x::l2) in
	renverse_ajoute l [];;

(*Tri par selection*)

(**
	[insertion] cette fonciton permet d'ajouter un entier dans une liste dans l'ordre croissant
	@param un int a placer dans la liste
	@param une liste d'entier tiré
	@return une liste trié*)
let rec insertion (n : int) (l : int list) : int list= 
	match l with 
	[] -> [n]
	| x :: r -> if n > x
								then x::insertion n r
else n :: x :: r;;
(**
		[tri_insertion] cette fonction permet de trié une liste dans l'ordre croissant grâce à l'algo de trie par insertion
		@param une liste d'entier
		@return une liste trié dans l'ordre croissant*)
let rec tri_insertion (l : int list) : int list = 
	match l with 
	[] -> []
	| x::r -> insertion x (tri_insertion r);;

 (*Recherche dans une liste d'associaiton*)

type association = int * string;;

type resultat = Trouve of string | PasTrouve

(**
		[cherche] cette fonction permet de cherché une cle dans une liste d'association cle valeur
		@param une liste de type somme résultat
		@param un int qui sert de clé
		@return un string
*)

let rec cherche (l : association list) (c : int): resultat = 
	match l  with
	[] -> PasTrouve
	| (u, v)::r -> if u == c 
									then Trouve v
								else cherche r c;;	 

(*Calculatrice en notation polonaise*)

type binop = Plus | Moins | Mult | Div;;
type elt_expr = Op of binop | Cst of int;;
type resultatE = Ok of int | ErrDivZero | ErrExpr;;

(**
		[eval_op] cette fonction permet de evaluer le resultat d'un operateur applique a des valeurs
		@param mon opérateur 
		@param cst 1
		@param cst 2
		@return resultat en mode resultat
*)

let eval_op (op : binop) (cst1 : resultatE) (cst2 : resultatE) : resultatE = 
	match (cst1, cst2) with
	(Ok(x), Ok(v)) -> (match op with
										|Plus -> Ok(x+v)
										|Moins ->Ok(x-v)
										|Mult ->Ok(x*v)
										|Div->if v == 0
														then ErrDivZero
									        else Ok(x/v))
	|(_,_) -> ErrExpr;;

(**
		[eval_expr] cette fonction permet devaluer une exprimer une expression issu d'une liste
		@param elt_expr list
		@return resultatE list
*)

let rec eval_expr (l : elt_expr list) : resultatE list =
	match l with
	[] -> []
	| [Cst(x)] -> [Ok x]
	| [Op(v)] -> [ErrExpr]
	| Op(x)::Cst(y)::Cst(z)::r -> (eval_op x (Ok(y)) (Ok(z))) :: eval_expr r
	| x::y::r ->[ErrExpr];;
