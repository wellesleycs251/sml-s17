(* Environment model interpreter for Bindex *)

use "../bindex/Bindex.sml";
use "../utils/Env.sml";
use "../utils/Utils.sml";

structure BindexEnvInterp = struct
  
  open Bindex

  exception EvalError of string

  (* val run : Bindex.pgm -> int list -> int *)
  fun run (Bindex(fmls,body)) ints = 
    let val flen = length fmls
	val ilen = length ints 
    in 
	if flen = ilen then 
            eval body (Env.make fmls ints)
	else 
            raise (EvalError ("Program expected " ^ (Int.toString flen)
                              ^ " arguments but got " ^ (Int.toString ilen)))
    end

  (* val eval : Bindex.exp -> int Env.env -> int *)
  and eval (Int i) env = i 
    | eval (Var name) env =
      (case Env.lookup name env of 
	   SOME(i) => i
         | NONE => raise (EvalError("Unbound variable: " ^ name)))
    | eval (BinApp(rator,rand1,rand2)) env = 
	(binopToFun rator)(eval rand1 env, eval rand2 env)
    | eval (Bind(name,defn,body)) env =
        eval body (Env.bind name (eval defn env) env) 

  (* val binopToFun : Bindex.binop -> (int * int) -> int *)
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

  (* An interactive read-eval-print loop (REPL) for Bindex expressions.
     By default, assumes zero arguments, but this can be changed
     with the #args directive (see below). The following directives
     are supported:

     + (#args (a_1 i_1) ... (a_n i_n)): Installs the n integers i_ 1 ... i_n 
       as the current program arguments a_1 ... a_n

     + (#run <pgm> <arg1> ... <arg_n>) runs the program specified
       by <filename> on the arguments in <args>, where

       - <pgm> is a symbol or string naming a file containing the program
         or an sexp representation of the program.

       - <arg_i> are integer program arguments

       E.g., (#run squares.bdx 7 5)
             (#run "squares.bdx" 7 5)
             (#run (bindex (x y) (/ (+ x y) 2)) 5 15)

     + (#quit): Exit the interpreter
   *)
  
  fun repl () =

    let

	fun println s = print (s^"\n")
		
	(* sexpToInt : sexp -> int *)
	fun sexpToInt (Sexp.Int i) = i
	  | sexpToInt sexp = raise (Fail ("Not an int!: " ^ (Sexp.sexpToString sexp)))

	(* sexpToStringIntPair : sexp -> (string * int) *)
	fun sexpToSymIntPair (Sexp.Seq [Sexp.Sym s, Sexp.Int i]) = (s, i)
	  | sexpToSymIntPair sexp = raise (Fail ("Not an symbol/int pair!"
						 ^ (Sexp.sexpToString sexp)))

	(* getPgm : sexp -> pgm *)				    
	(* get a Bindex program from a specification *)
	(* treat a symbol or string as the name of a file containing the program *)
	fun getPgm (Sexp.Sym filename) = fileToPgm filename
	  | getPgm (Sexp.Str filename) = fileToPgm filename
	  | getPgm sexp = sexpToPgm sexp 

	fun loop env = 
	    let val _ = print "\nbindex> " 
		val sexp = Sexp.readSexp()
	    in case sexp of 
		   Sexp.Seq [Sexp.Sym "#quit"] => println "Moriturus te saluto!"
		 | Sexp.Seq ((Sexp.Sym "#args") :: bindingxs) => 
		   let val (names, ints) = ListPair.unzip (map sexpToSymIntPair bindingxs)
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

end	
