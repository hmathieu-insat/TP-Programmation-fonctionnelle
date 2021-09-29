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

(* Reverse maps list lst with fun f *)
let rec rev_map f lst =
  let rec loop lst acu =
    match lst with
      | [] -> acu
      | hd::tl -> loop tl ((f hd)::acu)
  in
    loop lst []
;;
rev_map (fun x -> 2*x) [1;2;3];;
(* Tested and verified *)


(* Applies f to each element of lst in sequence *)
let rec iter f lst =
  match lst with
    | [] -> ()
    | hd::tl -> f hd ; iter f tl
;;
iter (Printf.printf "Element : %d\n%!") [ 2 ; 4 ; 6 ; 8 ];;
(* Tested and verified*)


(* Returns string of elements of list seperated by sep *)
let print_list sep conv lst =
  let rec loop lst acu =
    match lst with
      | hd::[] -> (conv hd)
      | hd::tl -> acu ^ (conv hd) ^ sep ^ (loop tl acu)
  in
    loop lst ""
;;
print_list ", " string_of_int [ 4 ; 8 ; 99 ];;
print_list " ++ " (fun x -> x) [ "aa" ; "bb" ; "cc" ];;
(* Tested and verified *)


(* Applies any operation op to every element of the list lst *)
let rec fold op acu = function
  | [] -> acu
  | hd::tl -> fold op (op acu hd) tl
;;
fold (fun a b -> a ^ " " ^ string_of_int b) "" [ 1 ; 2 ; 3 ; 4];;
fold (+) 0 [ 1 ; 2 ; 3 ; 4 ];;
fold ( * ) 1 [ 1 ; 2 ; 3 ; 4 ];;
fold (fun a b -> if a < b then b else a) 0 [ 10 ; 40 ; 20 ; 30 ];;
fold (fun a b -> b :: a) [] [ 4 ; 3 ; 2 ; 1 ];;
(* Tested and verified *)


(*Returns true if there exixts at least 1 element satisfying pred in lst *)
let exists pred = fold (fun a b-> a || (pred b)) false;;
exists (fun x -> x < 10) [ 20 ; 5 ; 30 ];;
exists (fun x -> x < 10) [ 20 ; 40 ; 30 ];;
exists (fun x -> x < 10) [];;
(* Tested and verified *)


(* Same function with different code *)
let rec exists_alt pred = function
  | [] -> false
  | hd::tl -> begin match (pred hd) with
      | false -> exists_alt pred tl
      | true -> true
    end
;;
exists (fun x -> x < 10) [ 20 ; 5 ; 30 ];;
exists (fun x -> x < 10) [ 20 ; 40 ; 30 ];;
exists (fun x -> x < 10) [];;
(* Tested and verified *)


(* Composes 2 functions *)
let (++) f g x= f(g(x));;
((fun x -> x - 10) ++ abs) (-20);;
(abs ++ (fun x -> x - 10)) (-20);;
(* Tested and verified *)

(* Returns true if pred is true forall elements of lst *)
let forall pred lst =  (exists (not ++ pred) lst) == false;;
forall (fun x -> x < 10) [ 20 ; 5 ; 30 ];;
forall (fun x -> x < 10) [ 20 ; 40 ; 30 ];;
forall (fun x -> x < 10) [];;
(* Tested and overall functional, [] empty list result is weird *)



(* - Exercise: Association lists -*)

