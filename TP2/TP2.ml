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

