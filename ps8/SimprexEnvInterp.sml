use "../ps8/Simprex.sml";
use "../utils/Env.sml";
use "../utils/Utils.sml";

structure SimprexEnvInterp = struct
  
  open Simprex

  exception EvalError of string

  (* val run : Simprex.pgm -> int list -> int *)
  fun run (Simprex(fmls,body)) ints = 
    let val flen = length fmls
	val ilen = length ints 
    in 
	if flen = ilen then 
            eval body (Env.make fmls ints)
	else 
            raise (EvalError ("Program expected " ^ (Int.toString flen)
                              ^ " arguments but got " ^ (Int.toString ilen)))
    end

  (* val eval : Simprex.exp -> int Env.env -> int *)
  and eval (Int i) env = i 
    | eval (Var name) env =
      (case Env.lookup name env of 
	   SOME(i) => i
         | NONE => raise (EvalError("Unbound variable: " ^ name)))
    | eval (BinApp(rator,rand1,rand2)) env = 
	(binopToFun rator)(eval rand1 env, eval rand2 env)
    | eval (Bind(name,defn,body)) env =
      eval body (Env.bind name (eval defn env) env)
    | eval (Sigma(name, lo, hi, body)) env =
      let val vlo = eval lo env
	  val vhi = eval hi env
	  val ints = Utils.range vlo (vhi + 1)
	  val vals = List.map (fn i => eval body (Env.bind name i env)) ints
      in List.foldr op+ 0 vals
      end
    (* Replace the followint stub for simprec expressions *)
    | eval putSimprecPatternHere env = 42 

  (* val binopToFun : Simprex.binop -> (int * int) -> int *)
  and binopToFun Add = op+
    | binopToFun Mul = op*
    | binopToFun Sub = op-
    | binopToFun Div = (fn(x,y) => if y = 0 
				   then raise (EvalError ("Division by 0: " 
							  ^ (Int.toString x)))
				   else x div y)
    | binopToFun Rem = (fn(x,y) => if y = 0 
				   then raise (EvalError ("Remainder by 0: " 
							  ^ (Int.toString x)))
				   else x mod y)

  (* A function for running programs expressed as strings *)
  fun runString pgmString args = 
    run (stringToPgm pgmString) args	  

  (* A function for running a programs in a files *)
  fun runFile pgmFile args = 
    run (fileToPgm pgmFile) args

  (* An interactive read-eval-print loop (REPL) for Simprex expressions.
     By default, assumes zero arguments, but this can be changed
     with the #args directive (see below). The following directives
     are supported:

     + (#args (a_1 i_1) ... (a_n i_n)): Installs the n integers i_ 1 ... i_n 
       as the current program arguments a_1 ... a_n

     + (#runFile <pgm> <arg1> ... <arg_n>) runs the program specified
       by <filename> on the arguments in <args>, where

       - <pgm> is a symbol or string naming a file containing the program
         or an sexp representation of the program.

       - <arg_i> are integer program arguments

       E.g., (#runFile squares.bdx 7 5)
             (#runFile "squares.bdx" 7 5)
             (#runFile (simprex (x y) (/ (+ x y) 2)) 5 15)

     + (#quit): Exit the interpreter
   *)
  
  fun repl () =

    let

	fun println s = print (s^"\n")
		
	(* sexpToInt : sexp -> int *)
	fun sexpToInt (Sexp.Int i) = i
	  | sexpToInt sexp = raise (Fail ("Not an int!: " ^ (Sexp.sexpToString sexp)))

	(* sexpToStringIntPair : sexp -> (string * int) *)
	and sexpToSymIntPair (Sexp.Seq [Sexp.Sym s, Sexp.Int i]) = (s, i)
	  | sexpToSymIntPair _ = raise (Fail "Not a symbol/int pair!")

	(* getPgm : sexp -> pgm *)				    
	(* get an intex program from a specification *)
	(* treat a symbol or string as the name of a file containing the program *)
	and getPgm (Sexp.Sym filename) = fileToPgm filename
	  | getPgm (Sexp.Str filename) = fileToPgm filename
	  | getPgm sexp = sexpToPgm sexp 

	and loop env = 
	    let val _ = print "\nsimprex> " 
		val sexp = Sexp.readSexp()
	    in case sexp of 
		   Sexp.Seq [Sexp.Sym "#quit"] => println "Moriturus te saluto!"
		 | Sexp.Seq ((Sexp.Sym "#args") :: bindings) => 
		   let val (names, ints) = ListPair.unzip (map sexpToSymIntPair bindings)
		   in loop (Env.make names ints)
		   end
		 | Sexp.Seq ((Sexp.Sym "#run") :: pgmx :: intxs) => 
		   let val _ = println (Int.toString (run (getPgm pgmx) (List.map sexpToInt intxs)))
			       handle EvalError s => println ("Error: " ^ s)
				    | SyntaxError s => println ("Error: " ^ s)
				    | Fail s => println ("Error: " ^ s)
				    | other => println ("Error: " ^ (exnName other)
							^ " -- " ^ (exnMessage other))
		   in loop env
		   end
		 | _ => let val  _ = println (Int.toString (eval (sexpToExp sexp) env))
				     handle EvalError s => println ("Error: " ^ s)
					  | SyntaxError s => println ("Error: " ^ s)
					  | Fail s => println ("Error: " ^ s)
					  | other => println ("Error: " ^ (exnName other)
							      ^ " -- " ^ (exnMessage other))
			in loop env
			end
	    end
    in loop Env.empty
    end

  (* Testing of simprex programs from files *)
  fun testEvaluator (simprexFileName, listOfArgListsAndResults) =
    let val _ = print ("-------------------------------------------------\n"
		       ^ "Testing Simprex program file "
		       ^ simprexFileName ^ "\n\n")
	val simprexPgm = fileToPgm(simprexFileName)
	val _ = print ("Simplrec program:\n"
		       ^ (pgmToString simprexPgm) ^ "\n\n")
	fun intsToString ints = "[" ^ (String.concatWith
					   ","
					   (map Int.toString ints)) ^ "]"
	fun testBehavior(args, expectedResult) =
	  let val _ = print ("Testing args " ^ (intsToString args) ^ ": ")
	      val expectedResultString = Int.toString expectedResult
	      val actualResultString = Int.toString (run simprexPgm args)
				       handle SyntaxError msg =>
					      ("Syntax error: " ^ msg)
					    | EvalError msg =>
					      ("Eval error: " ^ msg)
					    | exn => (exnName exn) ^ ": "
						     ^ (exnMessage exn)
	  in if actualResultString = expectedResultString
	     then print ("program returned expected result " ^ expectedResultString ^ "\n\n")
	     else print ("*** EVALUATION ERROR **"
			 ^ "\n  Expected result: " ^ expectedResultString
			 ^ "\n  Actual result: " ^ actualResultString
			 ^ "\n\n")
	  end
    in List.app testBehavior listOfArgListsAndResults
    end

(* (* Uncomment this section to test your changes to eval *)

  (* Evaluation tests *)
  val tests =       
      List.app testEvaluator
               [("../ps8/expt.spx", [([2,3],8),([3,2],9),([2,5],32),([5,2],25)]),
		("../ps8/fact.spx", [([1],1),([2],2),([3],6),([4],24),([5],120)]), 
		("../ps8/sos.spx", [([1],1),([2],5),([3],14),([4],30),([5],55)]),
   	        ("../ps8/sub.spx", [([0],0),([1],1),([2],1),([3],2),([4],2),([5],3)]),
		("../ps8/horner.spx", [([2,0],2),([2,1],3),([2,2],4),([2,3],5),([2,4],6),([2,5],7),
				       ([3,0],3),([3,1],6),([3,2],11),([3,3],18),([3,4],27),([3,5],38),
				       ([4,0],4),([4,1],10),([4,2],26),([4,3],58),([4,4],112),([4,5],194)]),
		("../ps8/poly.spx", [([0,1],1),([1,1],2),([2,1],3),([3,1],4),([4,1],5),
				     ([0,2],1),([1,2],3),([2,2],7),([3,2],15),([4,2],31),
				     ([0,3],1),([1,3],4),([2,3],13),([3,3],40),([4,3],121),
				     ([0,~2],1),([1,~2],~1),([2,~2],3),([3,~2],~5),([4,~2],11)])]	

 *)
	
end

