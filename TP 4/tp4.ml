(* --- TYPE STRUCTURES --- *)

(* -- RECORDS -- *)

(** Functions are first-class, they can appear in records. **)
type test =
    { (* A function which should be tested. *)
      fonc: (int -> int) ;
      
      (* An argument, which will be given to the function. *) 
      arg: int ;
      
      (* The expected result. *)
      expect: int }
;;


let apply t = t.fonc t.arg = t.expect ;;

let test1 = { fonc = (fun a -> 2 * a) ; arg = 2 ; expect = 4 };;

apply test1;;

let test2 = {test1 with arg = 5};;

apply test2;;


(* -- PARAMETERIZED TYPES -- *)

(* A parameterized record type. *)
type 'a testT =
    { fonc: ('a -> int) ;
      arg: 'a ;
      expect: int }
;;


let test1 = { fonc = (fun a -> 2 * a) ; arg = 2 ; expect = 4 }
and test2 = {test1 with arg = 5};;

let test3 = {fonc = (fun a -> 89) ; arg = true ; expect = 89 };;
apply test3;;

(* To be looked at again *)



(* -- ARRAYS -- *)


let foo a i = a.(i);;
let bar a i v = a.(i) <- v;;


(* -- MUTABLE RECORDS -- *)

type player =
    { name: string ;
      age: int ;
      mutable points: int }
;;


let show_player p =
  Printf.printf "Player %s , %d, with points %d is entering the court \n %!" p.name p.age p.points
;;

let new_player name age = {name = name ; age = age ; points = 0};;


let w = new_player "wesh" 22;;
show_player w;;


let add_points p points = p.points <- p.points + points;; 


add_points w 800;;
show_player w;;



let m = new_player "Mouloud" 35;;
show_player m;;
show_player w;;
add_points w 10 ; add_points w 20;;
show_player w;;
add_points m 30 ; add_points m 50;;
show_player m;;


type iplayer = {
  iname: string;
  iage: int;
  ipoints: int}
;;

let show_iplayer p =
  Printf.printf "Player %s , %d, with points %d is entering the court \n %!" p.iname p.iage p.ipoints
;;

let new_iplayer name age = {iname = name ; iage = age ; ipoints = 0};;

let add_ipoints p points = {p with ipoints = p.ipoints + points};;



let test () =
  (* Create an iplayer and add some points. *)
  let p1_a = new_iplayer "malid" 92 in
  let p1_b = add_ipoints p1_a 50 in
  let p1_c = add_ipoints p1_b 12 in
    show_iplayer p1_c
;;

test ();;

let create x = { contents = x };;
let read rf = rf.contents;;
let write rf v = rf.contents <- v;;

let test () = 
  let elt = create 10 in
    write elt 1; write elt 1 ;
    read elt
;;


test ();;

let r = ref 10;;

!r;;

r := 100;;
!r;;

(* - Exercise: references - *)
let gen1 =
  fun () -> 0
;;

let gen2 =
  fun () ->
  let count = ref 0 in
    count := !count + 1 ;
    !count
;;

let gen3 =
  let count = ref 0 in
    fun () ->
      count := !count + 1 ;
      !count
;;


gen1 ();;
gen2();; gen2();; gen2();;
gen3();;gen3();;gen3();;

count := 5;;

(* Impossible to modify count outisde of gen3 *)


(* -- VARIANT DATATYPES *)

type role = Player | Referee;;
type color = White | Yellow | Green | Blue | Red;;

type role = Player of color * int | Referee;;

let role1 = Referee
let role2 = Player (Green, 8)
let role3 = Player (Yellow, 10)

let get_number = function
  | Referee -> 0
  | Player (_, nb) -> nb


type people =
    { name: string ;
      role: role ;
      age: int }


let pla = {name = "Playah" ; role = Player (White, 2) ; age = 23 };;
let plou = {name = "Playouh" ; role = Player (Red, 15) ; age = 19 };;


let get_col = function
  | Player(_,_) -> true
  | _ -> false
;;


let same_team a b =
  match (a.role,b.role) with
    | (Player(x,_) , Player(y,_)) ->
        begin x == y end
    | _ -> false
;;

same_team pla plou;;



let is_number p n =
  match p.role with
    | Player (_, x) ->
        begin x == n end
    | _ -> false
;;

is_number pla 8;;


type 'a mylist = Empty | Cell of 'a * 'a mylist


(* -- PARAMETERIZED AND RECRUSIVE VARIANTS -- *)


(* Returns the 1st element of a list *)
let myhd = function 
  | Empty -> failwith "empty list"
  | Cell (a, _) -> a
;;

(* Returns the tail of list if possible *)
let rec mytl = function
  | Empty -> failwith "empty list"
  | Cell(a, Empty) -> a
  | Cell(a, c) -> mytl c
;;

(* Returns the length of a list w/o an accumulator *)
let rec mylength = function 
  | Empty -> 0
  | Cell (a , c) -> 1 + mylength c
;;

(* Returns the length of a list using an acumulator *)
let rec mylength1 acu = function
  | Empty -> acu
  | Cell (a, c) -> mylength (acu+1) c
;;

(* Returns the length of a list tail-recursively w/O accu *)
(* let myLength2 1 =
   let rec loop acu 1 =
   function
   | Empty -> acu
   | Cell(a, c) -> loop (acu+1) 2
   in
   loop 0 *)

(* - Exercise: Option type *)

let l = [1;2;3]
let t = [];;

let ohd = function
  | [] -> None
  | x::_ -> Some x
;;

let rec otl = function
  | [] -> None
  | a::[] -> Some a
  | a::l -> otl l

;;

ohd l;; ohd t;;
otl l;;otl t;;


(* -- EXERCISES ON LISTS -- *)

(* - Exercise: AD-hoc functions *)
type people =
    { name: string ;
      role: role ;
      age: int }
;;


let rec get_referees = function
  | [] -> []
  | a::l -> 
      begin match a.role with
        | Referee -> a::(get_referees l)
        | _ -> get_referees l
      end 
;;

let tlist = [{name = "a" ; role = Referee ; age = 44} ; {name = "b" ; role = Player(Red, 2); age = 43} ; {name = "c" ;  role = Referee ; age = 66} ];;

get_referees tlist;;
(* Tested and functional *)

let rec get_younger plist ag = 
  match plist with
    | [] -> []
    | a::l ->
        begin match a.age with
          | z when (z <= ag) -> a::(get_younger l ag)
          | _ -> get_younger l ag
        end
;;

get_younger tlist 70;;
(* Tested and functional *)


let rec find_color plist col =
  match plist with
    | [] -> None
    | p::l -> 
        begin match p.role with
          | Player(c,_) when c == col -> Some p
          | _ -> find_color l col
        end
;;

find_color tlist Red;;
(* Tested and verified *)


(* - Exercis: generic functions - *)
let rec filter pred l = 
  match l with
    | [] -> []
    | a::l -> 
        begin match (pred a) with
          | true -> a::(filter pred l)
          | false -> filter pred l
        end 
;;

filter (fun x -> x mod 2 = 0) [ 1 ; 2 ; 3 ; 4 ; 5 ; 6];;

filter (fun x -> x < 10) [ 100 ; 5 ; 15 ; 6 ; 16 ; 7 ];;

filter (fun x -> x < 0) [ 100 ; 5 ; 15 ; 6 ; 16 ; 7 ]
;;


let get_referees = filter (fun p -> p.role == Referee);;
get_referees tlist;;

let get_younger ag = filter (fun p -> p.age <  ag);;
get_younger 70 tlist;;


let rec find pred lst =
  match lst with
    | [] -> None
    | a::l -> 
        begin match (pred a) with
          | true -> a
          | false -> find pred l
        end
;;


let has_color col p = 
  match p.role with
    | Player(c, _) when c == col -> true
    | _ -> false
;;

let find_color col = (fun p -> (has_color col p)) ;;







   
