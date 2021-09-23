
(* -- BASIC PATTERN MATCHING -- *)

let f x =
  match x with
    | 0 -> "zer"
    | _ -> "one"
    | 2 -> "two"
    | 1 -> "else"
;;

(* '_' should be the last case in the match *)


(* Pattern matching with bool, no '_' case needed *)
let g = function
  | true -> "true"
  | false -> "false"
;;

g false;;


(* Pattern matching with strings, '_' case required *)
let h = function
  | "who" -> "is but the form"
  | "following" -> "the function of what"
  | _ -> "and what I am is a man in a mask"
;;

h "who";;
h "following";;
h "toto";;


(* Pattern matching tuples *)
let tup = function
  | (0, true) -> 1
  | (0, false) -> -1
  | (_, false) -> 2
  | ( _, true) -> 3
;;

tup (0, false);;
tup (19, true);;
(* All cases of tuple (int, boolean) are here considered. The pattern matching is exhaustive in this regard *)
(* Should one want to change the 3rd line to | (1, false) changing the exhauvetiveness of the pattern matching, the compilator would issue a warning *)



(* - Difference between match and function - *)
let confused2 (a,b) = function
  | (0, true) -> 1
  | (n, _) -> 10
;;
(* The type of this function is a * b -> int * bool -> int, the tup (a,b) is not the tup being matched *)
confused2 (1,2) (0, true);;
confused2 (1,2) (4, true);;


(* Variable binding patterns *)
let calc = function
  | (x,y, "add") -> x + y
  | (x,y, "sub") -> x - y
  | (x,y, "mul") -> x * y
  | (x,0, "div") -> failwith "Divided per 0"
  | (x,y, "div") -> x / y
  | _ ->  failwith "unknown operator"
;;


calc (110, 220, "add");;


(* Start of the curried version of calc *)
let calc2 x y op = match (x,y, op) with
  | (x, y, "add") -> x + y
  | (x,y, "sub") -> x - y
  | (x,y, "mul") -> x * y
  | (x,0, "div") -> failwith "Divided per 0"
  | (x,y, "div") -> x / y
  | _ ->  failwith "unknown operator"
;;

calc2 10 5 "sub";;
calc2 10 5 "div";;


(* - Exercise: basic pattern matching - *)
let xor a b = match (a,b) with 
  | (true, true) -> false
  | (true, false) -> true
  | (false, true) -> true
  | (false, false) -> false
;;

xor true false;;
xor false false;;

type operation = Add | Sub | Mul | Div;;

let calc3 x y op = match (x,y,op) with
  | (x, y, Add) -> x + y
  | (x,y, Sub) -> x - y
  | (x,y, Mul) -> x * y
  | (x,0, Div) -> failwith "Divided per 0"
  | (x,y, Div) -> x / y
;;

calc3 4 5 Mul;;



(* -- SEQUENCE -- *)

let f x =
  Printf.printf "Calling f with val x = %d \n %!" x ;
  x * x + 2
;;

f 2;;


(* - Exercise: sequence - *)
let show f v =
  Printf.printf "Calling function with value %d \n %!" v ;
  f v
;;

let double x = 2 * x;;
show double 5;;


let pshow cv f v = 
  Printf.printf "Calling function with value %s \n %!" (cv v);
  f v
;;

let show_double = pshow string_of_int double;;
show_double 40;;



(* -- INNER LET -- *)

let f a p =
  let (b,c) = p in
    a+b+c
;;

f 5 (1,0);;

let f a (b,c) = a+b+c;;

let f a p =
  let (b,c) = p in
    a+b+c
;;
f 5 (1,0);;

let f a = fun (b,c) -> a+b+c;;

let get_triple (a ,(b,c)) = [ a ; b ; c ];;



(* -- EXPRESSIONS AND DEFINITIONS -- *)

let fancy1 x =
  begin
    if x = 0 then
      (fun a b -> a * b)
    else 
      (fun a b -> 0)
  end
    (x-1) (x+1)
;;


fancy1 0;;


let fancy2 a b c =
  (* Remember concatenation? *)
  "Hello " ^ 

  let f =
    if a = 0 then
      match(b,c) with
        | (1,0) -> (fun x -> b + c )
        | (0,1) -> (function 4 -> 10
                           | _ -> 12 )
    else
      fun a -> 2*a
  in
    (* Use f here *)
    string_of_int (f 5)
;;

fancy2 0 1 0;;
