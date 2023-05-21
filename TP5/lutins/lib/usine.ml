module MonUsine : Usine = struct
  
type jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche;;

let string_of_jour (j: jour) =
    match j with
    Lundi -> "Lundi"
    | Mardi -> "Mardi"
    | Mercredi -> "Mercredi"
    | Jeudi -> "Jeudi"
    | Vendredi -> "Vendredi"
    | Samedi -> "Samedi"
    | Dimanche -> "Dimanche";;
end