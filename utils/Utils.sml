structure Utils = struct

fun range lo hi = if lo >= hi then [] else lo :: (range (lo + 1) hi)

fun stringMake n chr = (* make a string with n copies of chr *)
  String.implode (map (fn _ => chr) (range 0 n))

fun uncurry2 fcn = fn (a,b) => fcn a b
fun uncurry3 fcn = fn (a,b,c) => fcn a b c

fun flip2 curriedBinop a b = curriedBinop b a				     
				   
fun fileToString name = 
  let val instream = TextIO.openIn name
      fun readLines linesSoFar =
	case TextIO.inputLine instream of
	    SOME line => readLines (line :: linesSoFar)
	  | NONE => String.concat(List.rev linesSoFar)
  in let val str = readLines []
	 val _ = TextIO.closeIn instream
     in str
     end
  end

fun stringToFile str filename =
  let val outstream = TextIO.openOut filename
      val _ = TextIO.output(outstream, str)
  in TextIO.closeOut outstream
  end

fun readLineFromConsole () =  Option.valOf (TextIO.inputLine TextIO.stdIn)

fun println s = print (s^"\n")

fun listMember elt xs = List.exists (fn x => x = elt) xs		      

(* fresh creates a "fresh" name for the given string
   by adding a "." followed by a unique number.
   If the given string already contains a dot, 
   fresh just changes the number. E.g., fresh "foo.17"
   will give a string of the form "foo.XXX" *)
val fresh =
    let val counter = ref 0
    in fn str =>
	   let val base = case List.find (fn (c,index) => c = #".")
					 (ListPair.zip(String.explode str, range 0 (String.size str))) of
			      SOME (c,i) => String.substring(str,0,i)
			    | NONE => str
	   in let val count = !counter 
                  val _ = counter := count + 1
	      in base ^ "." ^ (Int.toString count)
	      end
	   end
    end	       
  
end
