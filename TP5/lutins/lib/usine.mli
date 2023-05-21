module Usine = sig

(**
        Les jours de la semaine
*)
    type jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche
(**
    Donne une representation sous forme de string d'un jour
*)
    val string_of_jour : jour -> string
end