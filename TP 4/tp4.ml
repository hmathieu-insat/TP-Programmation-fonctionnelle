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


