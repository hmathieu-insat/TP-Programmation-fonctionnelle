let showdir () =
  let arr = Sys.readdir (Sys.getcwd()) in
    Array.sort compare arr;
    Array.iter (Printf.printf "%s \n") arr
;;

let () = showdir ();;

let a = Not_found;;

let a = 1;;

raise ;;
let a () = Not_found;;
let b () = raise Not_found;;



let a = 1::
exception Horrible_error;;
exception Bad_bad_thing of int * string;;

let take_two_opt l =
  try Some (take_two l)
  with Not_found -> None
;;


let test_raise x =
  try
    if x < 0 then raise Not_found
    else if x = 0 then failwith "Zero"
    else if x > 100 then raise Horrible_error
    else if x > 10 then raise (Bad_bad_thing (x, "Too big."))
    else [ string_of_int x ]
  with
    | Not_found -> []
    | Failure s -> [s]
;;
test_raise (-12);;
test_raise 0;;


let call f arg = try
    f arg
  with
    | e -> Printf.printf "%s" (Printexc.to_string e) ; raise e
;;



type 'a result = Ok of 'a | Error of exn;;

let eval f arg = try
    Ok(f arg)
  with
    | ex -> Error ex
;;


