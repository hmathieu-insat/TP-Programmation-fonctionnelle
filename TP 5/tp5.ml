(** --- TP 5: ALGORITHMS AND MORE STRUCTURE TYPES --- **)

(* -- Practicing on lists -- *)

(* - Exercise: 1st order functions on lists - *)

let rec nth lst n = 
  match lst with
    | [] -> failwith "List too small"
    | a::l -> 
        begin if n == 0 then
            a
          else
            nth l (n-1)
        end 
;;

let tlist = [1;2;3;4;5];;
nth tlist 2;;
(* Tested & Verified *)



let rev lst = 
  let rec f lst acu =
    match lst with
      | [] -> acu
      | a::l -> f l (a::acu)
  in 
    f lst []
;;
rev tlist;;

(* Standard append version *)
let append l1 l2 = 
  match l1 with 
    | hd::tl -> hd :: (append tl l2)
    | [] -> l2
;;
append [1;2] tlist;;



(* Tail-recursive append function *)
let rec rev_append l1 l2 = 
  match l1 with 
    | [] -> l2
    | hd::lt -> append lt (hd::l2)
;;
rev_append [1;2] tlist;;


(* - Exercise: Higher-order functions on lists - *)
let rec rev_map 
