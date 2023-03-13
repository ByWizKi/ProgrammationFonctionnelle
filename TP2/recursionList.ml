(*concatenation de liste*)

let rec concatene (l1 : int list) (l2 : int list) : int list =
	match l1 with 
	[] -> l2
	| x::r -> x::(concatene r l2);;

let rec applatit (l : (int list) list) : int list = 
	match l with
	[]->[]
	| x::r-> concatene x (applatit r);;

(*retournement de liste*)
let renverse (l:int list) : int list = 
	let rec renverse_ajoute (l1 : int list) (l2 : int list) : int list =
		match l1 with
		[]->l2
		| x::r -> renverse_ajoute r (x::l2) in 
		renverse_ajoute l [];;

(*Tri par insertion*)

let rec insertion (e: int) (l:int list) : int list = 
		match l with
		[] -> e::[]
		| x::r -> if e <= x 
								then e::l
							else x::(insertion e r);;
let tri_insertion (l:int list) : int list = 
	let rec tri_insertion_aux (l1: int list) (l2:int list) =
		match l1 with 
		[]->l2
		| x::r -> tri_insertion_aux r (insertion x l2) in
	tri_insertion_aux l [];;

(*Recherche dans une liste d'association*)
type association = A of int * string;;

type resultat_bis = PasTrouve 
								| Trouve ;;

let cherche (cle : int) (l : association list ) : resultat_bis = 
	match l with  
	[] -> PasTrouve
	| A(c,v)::r -> if c = cle 
										then Trouve
									else PasTrouve;;


(*Calculatrice Polonaise*)

type binop = Plus | Moins | Mult | Div;;
type elt_expr = Op of binop | Cst of int;;
type resultat = Ok of int | ErrDivZero | ErrExpr;;

let eval_op (b:binop) (c1 : resultat) (c2 : resultat) : resultat =
	match c1,c2 with
	| ErrDivZero, _ -> ErrExpr
	| ErrExpr, _ -> ErrExpr
	| _, ErrDivZero -> ErrExpr
	| _, ErrExpr -> ErrExpr
	| Ok(x), Ok(y) -> match b with
									| Plus -> Ok (x + y)
									| Moins -> Ok (x-y)
									| Mult -> Ok (x*y)
									| Div -> if x = 0 || y = 0
														then ErrDivZero
														else Ok (x/y);;
	 
(* let rec eval_expr (l : elt_expr list) : resultat list  =
	 match l with
	 [] ->  []
	 | Cst(x)::r -> Ok(x)::eval_expr r
	 | Op(x)::r -> eval_expr r;; *)
