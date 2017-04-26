use "../intex/Intex.sml";

structure IntexArgChecker =

struct

exception Unimplemented

open Intex

(* checkBottomUp: pgm -> bool *)
fun checkBottomUp (Intex(numargs,body)) =
  let val (min,max) = checkExpBottomUp body
  in 0 < min andalso max <= numargs
  end

(* val checkExpBottomUp : Intex.exp -> int * int *)
and checkExpBottomUp (Int i) = (valOf Int.maxInt, valOf Int.minInt)
  | checkExpBottomUp (Arg index) = raise Unimplemented
  | checkExpBottomUp (BinApp(_,exp1,exp2)) = raise Unimplemented

(* val checkTopDown: pgm -> bool *)
fun checkTopDown (Intex(numargs,body)) =
  checkExpTopDown numargs body

(* val checkExpTopDown : int -> Intex.exp -> bool *)
and checkExpTopDown numargs (Int i) = raise Unimplemented
  | checkExpTopDown numargs (Arg index) = raise Unimplemented
  | checkExpTopDown numargs (BinApp(_,exp1,exp2)) = raise Unimplemented

end

(* Test cases *)

open IntexArgChecker

val negativeIndex = Intex(3, BinApp(Mul,
				    BinApp(Add, Arg 1, Arg ~2),
				    Arg 3))

val tooLargeIndex = Intex(3, BinApp(Mul,
				    BinApp(Add, Arg 1, Arg 2),
				    BinApp(Sub, Arg 4, Arg 3)))

val passes = [sqr, avg, f2c, divRem]
val fails = [negativeIndex, tooLargeIndex]
val all = passes @ fails

(* Uncomment the following to check. 
   The expected result in both cases is 
   [true, true, true, true, false, false] *)		       
(* val topDownChecks = map checkTopDown all *)
(* val bottomUpChecks = map checkBottomUp all *)		       			

