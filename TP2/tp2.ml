let pmul x y = (x+y, x*y);;

let (u,v) = pmul 2 3 ;;


(* -- LAMBDAS (ANONYMOUS FUNCTIONS)-- *)
let mul2 x = x*2;;
let mul3 x = x*3;;

let list = [mul2 ; mul3];;


let f = fun (x,y,z) -> x-y*z ;;

f (1,2,3);;



(* Define mul2 and mul3 with a single let *)
let (mul2, mul3) = ((fun x -> 2 * x), (fun x -> 3 * x));;



(* -- POLYMORPHISM -- *)

let mk_pair (a:int) (b:int) = (a,b);;
let mk_pair a b = (a,b);;
mk_pair true false;; (* we can indeed see a, b are type variable i.e not bound to any type *)
(* ==> i.e polymorphic variables *)

let mk_list a b = [a ; b];;
(* mk_list is less polymorphic than mk_pair : a list being composed of elements all of a same type, a and b are bound to 1 type *)

fst;;
(* Based on function fst type ('a * 'b -> 'a), it is a curried function which returns the 1st element in a tuple *)

let fun_f a b c = if a then b else c;;
fun_f false 2.0 3.0;;
(* As it is impossible to b and c to have different types, the type of the function necessarily is : bool -> 'a -> 'a -> 'a *)

[];; (* ==> type 'a list *)


let f x = if x < 0 then None else Some x;;
f (-5);;
f 10;;


(* A few polyphormic functions *)
(=);;
(==);;
(<>);;
(!=);;

(* Comparing equal lists *)
let list1 = [1;2;3];;
let list2 = [1;2;3];;

list1 = list2;;
list1 == list2;;
(* '=' is the structural equality, returns true for 2 equal lists
   '==' is the physical equality and compares pointers, returns false for the same lists *)

(* Comparing equal functions *)
let f x = 2 * x;;
let f1 x = 2 * x;;

(*f = f1;;*)
f == f1;;
(* Exception raised while trying to use '=' between 2 functions
   False returned while comparing using '==' *)



(* -- RECURSIVE FUNCTIONS -- *)

(* Function returning the length of a given list *)
let rec length = function
  | [] -> 0
  | x :: rest -> 1 + length rest
;;
(* Length has a polyphormic arg type since it can return the length of a list of any type. *)

(* Function incrementing by 1 each element of a given list *)
let rec incr = function
  | [] -> []
  | x::rest -> (x + 1 ):: incr rest
;;

(* Same functions using accumulators *)
let rec length_bis acu = function
  | [] -> acu
  | x :: rest -> length_bis (acu + 1) rest

let rec incr_bis acu = function
  | [] -> acu
  | x :: rest -> incr_bis (x + 1 :: acu) rest



(* - Exercise: recursive functions *)
let rec count_ones acu = function
  | [] -> acu
  | x::rest -> if x == 1 then count_ones (acu+1) rest else count_ones acu rest
;;

count_ones 0 [ 1 ];;
count_ones 0 [ 1 ; 1 ; 1 ; 9 ; 1 ];;
count_ones 10 [ 1 ; 1 ; 1 ; 9 ; 1 ];;
(* Tests passed for count_ones w acu *)

let rec count_ones_bis = function
  | [] -> 0
  | x::rest -> if x == 1 then 1 + count_ones_bis rest else count_ones_bis rest
;;

count_ones_bis [ 1 ];;
count_ones_bis [ 1 ; 1 ; 1 ; 9 ; 1 ];;
(* Tests passed for count_ones w/o accumulator *)


let rec sum = function
  | [] -> 0
  | x::rest -> x + sum rest
;;

sum [];;
sum [8;1;9];;
sum [ 1 ; 1 ; 1 ; 9 ; 1 ];;
(* Tests passed for sum w/o accumulator *)


let rec sum_bis acu = function
  | [] -> acu
  | x::rest -> sum_bis (acu + x) rest
;;

sum_bis 0 [ 8 ; 9 ];;
sum_bis 0 [ 1 ; 1 ; 1 ; 9 ; 1 ];;
sum_bis 10 [ 1 ; 1 ; 1 ; 9 ; 1 ];;
(* Tests passed for sum w accumulator *)


let rec perms = function
  | [] -> []
  | (a,b)::rest -> (b,a) :: perms rest
;;

perms [];;
perms [ (1, true) ];;
perms [ ('a', 8) ; ('b', 7) ];;
(* Tests passed for perms w/o accumulator *)


let rec perms_bis acu = function
  | [] -> acu
  | (a,b)::rest -> perms_bis ((b,a)::acu) rest
;;

perms_bis [] [];;
perms_bis [] [(1, true)];;
perms_bis [] [('a', 8) ; ('b', 7)];;
(* Tests passed for perms w accumulator reversing the list *)



let rec mk_list = function
  | 0 -> []
  | x -> x::mk_list (x-1)
;;

mk_list 6;;
mk_list 0;;
(* Tests passed for mk_list w/o accu *)


let rec mk_aculist acu = function
  | 0 -> acu
  | x -> mk_aculist (x::acu) (x-1)
;;

mk_aculist [] 4;;
mk_aculist [] 0;;
mk_aculist [99] 5;;
(* Tests passed for mk_list w accu (the accu being returned at the end of the recursion *)

let l1 = mk_list 100000;;
let l1 = mk_list 270000;;
(* The size limit of mk_list is inbetween 270000 and 300000 list elements *)


let l2 = mk_aculist [] 10000000;;

sum l2;; (* Sum w/o accu fails whereas it works w accu *)
sum_bis 0 l2;;

count_ones_bis l2;; (* Count_ones has been optimized, succeeding both w & w/o accu *)


let st = Gc.quick_stat () in
  Printf.printf "Stack size = %d
%!" Gc.(st.stack_size) ;;



(* -- HIGHER ORDER FUNCTIONS -- *)

count_ones;;
sum;;
let fun_list = [count_ones ; sum ];;
(* The creation of a list containing both count_ones and sum failed as could be expected by thir different types *)

let fsum f = f 0 + f 1 + f 2 + f 3;;

let flist f = [f 0 ; f 1 ; f 2; f 3;];;
flist string_of_int ;;
flist (fun x -> x +1);;
flist (fun x -> x);;


(* - Exercise: Higher order functions - *)
let rec fsumlist f = function
  | [] -> 0
  | x::rest -> f x + fsumlist f rest
;;

fsumlist f [];;
fsumlist (fun x -> 2*x) [1;2;3];;


let rec fsumlist_acu f acu = function
  | [] -> acu
  | x::rest -> fsumlist_acu f (acu + x) rest
;;

fsumlist_acu (fun x -> 2*x) 0 [1;2;3];;
(* Tests are conclusive w & w/o acu for the fsumlist *)


fsumlist_acu (fun x -> 2*x) 0 [1;2;3];;



(* Exercise - Classical High-order, polymorphic functions *)
let rec map f = function
  | [] -> []
  | x::rest -> (f x)::(map f rest)
;;

map (fun x -> x + 1) [ 5 ; 10 ; 15 ];;
map string_of_int [ 5 ; 10 ; 15 ];;
(* Tests are conclusive for the function map *)

map (fun x -> (x, string_of_int x)) [ 5 ; 10 ; 15 ];;


(* Predicate find *)
let rec find f = function
  | [] -> raise Not_found
  | x::rest -> if (f x) == true then x else find f rest
;;


find (fun x -> x > 10) [ 5 ; 12 ; 7 ; 8 ];;

find (fun x -> true) [ 5 ; 10 ; 15 ];;
(* Tests conclusive for predicate find *)



(* - Exercise: Map on options - *)

let omap f = function
  | None -> None
  | Some x -> Some (f x)
;;

omap (fun x -> true) None;;
omap (fun x -> true) (Some 10);;
omap (fun x -> not x) (Some true);;
omap (fun x -> not x) None;;
omap (fun x -> x +1 ) None;;
omap (fun x -> x +1 ) (Some 100);;
(* Tests conclusive for map on options *)







