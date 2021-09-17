let pmul x y = (x+y, x*y);;

let (u,v) = pmul 2 3 ;;

let mul2 x = x*2;;

let mul3 x = x*3;;


let list = [mul2 ; mul3];;

let mul2 = fun x -> x * 2 ;;

let f = fun (x,y,z) -> x-y*z ;;

f (1,2,3);;



