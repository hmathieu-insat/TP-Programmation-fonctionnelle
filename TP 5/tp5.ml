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
let assoc1 = [ ("Lucy", false) ; ("Mike", false) ; ("Hilary", false) ; ("Donald", true) ];;

(* Researches value associated to key in list *)
let rec assoc key = function
  | [] -> raise Not_found
  | hd::tl -> begin match hd with
      | (k,v) when k = key -> v
      | _ -> assoc key tl
    end
;;
assoc "Donald" assoc1;;
assoc "Mike" assoc1;;

let a = [1;2];;
1::a;;

(* Remove key from assocation list *)
let rec remove_assoc key lst =
  match lst with
    | [] -> []
    | hd::tl -> begin match hd with
        | (k,v) when k = key -> remove_assoc key tl
        | _ -> hd::(remove_assoc key tl)
      end
;;

remove_assoc "Donald" assoc1;;


(** --- TREES --- **)

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;

(* Returns the max depth of tree*)
let rec depth tree =
  match tree with 
    | Leaf(_) -> 0
    | Node(a,_) -> 1 + (depth a)
;;
depth (Leaf 100);;
depth (Node (Leaf 100, Leaf 200));;

(*Builds a tree of n depth and x value in leaves *)
let rec build n x =
  match n with
    | 0 -> Leaf x
    | _ -> Node (build (n-1) x , build (n-1) x)
;;
let t = build 5 3;;
depth t;;

let print_tree tos tree =
  let rec loop margin = function
    | Leaf x -> Printf.printf "___ %s\n%!" (tos x)
    | Node (a,b) ->
        Printf.printf "____" ;
        loop (margin ^ "|   ") a ;
        Printf.printf "%s|\n%s|" margin margin ;
        loop (margin ^ "    ") b
  in
    loop "   " tree
;;
print_tree string_of_int t;;


let build_fold n init f =
  let rec aux n x =
    match n with
      | 0 -> (Leaf x, f x)
      | _ -> let (g,j) = aux (n-1) x in
          let (d,k) = aux (n-1) j in
            (Node (g,d), k)
  in
  let (a,_) = aux n init in
    a
;;

let tree1 = build_fold 3 10 (fun x -> x+2);;
print_tree string_of_int tree1;;


let tree2 = build_fold 2 "o" ( fun x -> "(" ^ x ^ ")" );;


let rec tmap f = function
  | Leaf x -> Leaf (f x)
  | Node (a,b) -> Node(tmap f a, tmap f b)
;;


let rec tfind pred = function
  | Leaf x -> begin match pred x with
      | true -> Some x
      | false -> None
    end
  | Node (a,b) -> if Option.is_some(tfind pred a) then tfind pred a else tfind pred b
;;
tfind (fun x -> x == 20) tree1;;


let rec contains v = function
  | Leaf x -> if x == v then true else false
  | Node (a,b) -> if contains v a then contains v a else contains v b
;;
contains 22 tree1;;


let rec replace pred sub tree =
  if pred tree then sub else match tree with
    | Leaf x -> Leaf x
    | Node (a,b) -> Node(replace pred sub a, replace pred sub b)
;;

let t1 =replace (fun t -> contains 14 t && depth t = 1) (Leaf 0) tree1;;
let pt = print_tree string_of_int;;
pt t1;;
