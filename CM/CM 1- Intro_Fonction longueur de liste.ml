(* Définition de listes et calcul de la longueur *)


type intlist = Nil | Cell of int * intlist;;

let rec length l =
  match l with
    | Nil -> 0
    | Cell  (x, t) -> x + length t
;;

