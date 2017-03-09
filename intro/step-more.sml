(* This is the contents of the file step-more.sml *)

fun printPair (a,b) =
  print ("(" ^ (Int.toString a) ^ ","
	  ^ (Int.toString b) ^ ")\n")

fun stepUntilPrint ((a,b), limit) =
  if a >= limit then
    (a,b)
  else
    (printPair (a,b); (* here, semicolon sequences expressions *)
     stepUntilPrint (step(a,b), limit))
