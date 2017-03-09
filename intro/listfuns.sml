(* matchtest : (int * int) list -> (int * int) list *)
fun matchtest xs =
  case xs of 
    [] => []
  | [(a,b)] => [(b,a)]
  | (a,b) :: (c,d) :: zs => (a+c,b*d) :: (c,d) :: zs

fun matchtest2 xs =
  case xs of 
    [] => []
  | [(a,b)] => [(b,a)]
  | (a,b) :: (ys as ((c,d) :: zs)) => (a+c,b*d) :: ys
    (* subpatterns can be named with “as” *)

fun matchtest3 [] = [] 
  | matchtest3 [(a,b)] = [(b,a)]
  | matchtest3 ((a,b) :: (ys as ((c,d) :: zs))) 
      (* parens around pattern necessary above *)
    = (a+c,b*d) :: ys 

(* myMap : ('a -> 'b) -> 'a list -> 'b list *)
fun myMap f [] = [] 
  | myMap f (x::xs) = (f x)::(myMap f xs)

(* myFilter : ('a -> bool) -> 'a list -> 'a list *)
fun myFilter pred [] = []
  | myFilter pred (x::xs) = 
      if (pred x) then
	  x :: (myFilter pred xs)
      else
	  (myFilter pred xs)



