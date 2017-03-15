(* CS 251: ML Modules and Abstract Data Types *)

signature MATHLIB =
sig
    val fact : int -> int
    (* val doubler : int -> int *) (* can hide bindings from clients *)
    val half_pi : real
    val twelve : int		      

end

structure MyMathLib :> MATHLIB =
struct
    fun fact 0 = 1
      | fact x = x * fact (x - 1)

    fun doubler y = y + y
                      
    val half_pi = Math.pi / 2.0

    val twelve = doubler (fact 3)

end
                                     
val pi = MyMathLib.half_pi * 2.0
val four = MyMathLib.twelve div 3

                                                  




