let qDP (l: int list) : int list = List.filter (fun x -> (x mod 2) = 0) l;;

let inter (x : int) (y : int) = List.filter(fun z -> (z mod x = 0) && z >= 2) (List.init (y+1) (fun n -> n));;

let eD (x : int) (l : int list) : bool = List.exists(fun y -> x mod y = 0) l;;

let eM (x: 'a) (l : 'a  list) : bool = List.for_all (fun y -> x > y) l && List.exists (fun z -> z = x) l;;

let au2 (l : int list) : int list = List.map (fun x -> x*x) l;;

let add1 (x : int) l : int list = List.map (fun y -> x+y) l;;

let transforme (l : int list) : string =  List.fold_left (^) "" (List.map (fun x -> (string_of_int x)) l);;

let rev : int list -> int list = 
  List.fold_left (fun acc x -> x::acc) [];;

let map (f: 'a -> 'b ) (l : 'a list) : 'b list = List.fold_right (fun x acc  -> (f x)::acc) l [];;

let filter (f: 'a -> bool) (l : 'a list) : 'a list = List.fold_right (fun x acc -> if (f x) then x::acc else acc) l [];;

let transforme2 (l : int list) : string =  List.fold_left (^) "" (List.map (fun x -> (string_of_int x) ^ " ") l);;
let chaine (x : int) : string = transforme2 (inter 1 x);;
