(*Parcours et transformation de structure*)


(**
    [only_pair] cette fonction prend une liste en argument et renvoie que les int paires
    @param l une liste de int
    @return une liste de int paire
*)

let only_pair (l : int list) : int list = List.filter (fun x -> x mod 2 = 0) l;;
assert (only_pair [1;2;3;4;5;6;7;8] = [2;4;6;8]);;

(**
    [built_int_list] cette fonction permet de creer une liste d'entier a partir de x et y
    @param x un entier de départ
    @param y un entier d'arriver
    @return un liste compris en x et y
*)

let built_int_list (x: int) (y : int) : int list= List.filter (fun z -> z mod x = 0) (List.init (y-1) (fun z -> z+2));;
assert(built_int_list 3 20 = [3;6;9;12;15;18]);;

(**
    [is_divisible] cette fonction permet de savoir si un entier est divisible dans une list au moins une fois
    @param liste de int
    @param un int
    @return vrai si un element de la liste divise sinon false
*)

let is_divisible (l : int list) (e : int) : bool = List.exists (fun x -> e mod x =0) l;;
assert(is_divisible [3;5;7] 2 = false);;

(**
    [is_max] cette fonction permet de savoir si un element est le max d'une liste
    @param une liste de int
    @param un int
    @return vrai si il est le plus grand de la liste
*)

let is_max (l : int list) (e : int) : bool = List.exists (fun x -> if max e x = e then true else false) l;;
assert(is_max [1;2;3;4;5;6;7;8;9] 0 = false);;

(**
    [all_square] cette fonction permet de mettre tous les elements d'une liste au carré
    @param une liste
*)

let all_square (l : int list): int list = List.map (fun x -> x*x) l;;
assert(all_square [2;4;6] = [4;16;36]);;

(**
    [add_list] cette fonction permet d'ajouter un int a tous les elements d'une liste
    @param un int
    @param une liste
    @return la liste ou tous ses elements on et ete ajoute par le int
*)

let add_list (e : int ) (l : int list) : int list= List.map(fun x -> x+e) l;;
assert(add_list 10 [2;2;2;2;2;2;2]=[12;12;12;12;12;12;12]);;

(**
    [string_of_int_list] cette fonction permet de tranformer une int list en chaine de caractere
    @param une int liste
    @return une string
*)

let string_of_int_list (l : int list) : string = List.fold_right (fun x acc -> string_of_int x ^ " " ^ acc) l "";;
assert(string_of_int_list [1;2;3;4;5;6;7] = "1 2 3 4 5 6 7 ");;

(**
    [rev_list] cette fonction permet de renverser une list
    @param une 'a list
    @return l'inverse de la liste
*)

let rev_list (l :'a list) : 'a list = List.fold_left(fun acc x -> x::acc) [] l;;
assert(rev_list [1;2;3;4;5] = [5;4;3;2;1]);;

(**
    [first_n_string] cette fonction permet de faire une string qui les n premiers entiers
    @param un int
    @return un string avec les entiers
*)

let first_n_string (n : int) : string = List.fold_right (fun x acc -> " " ^ string_of_int x ^ acc) (List.init n (fun x -> x+1)) "";;
assert(first_n_string 5 = " 1 2 3 4 5");;

(**
    [primes_list] cette fonction renvoie une liste avec les nombres premiers
    @param un int
    @return la liste des nombres premier jusqu'a n
*)
