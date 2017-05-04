(* Environment model interpreter for Valex *)

use "../valex/Valex.sml";
use "../utils/Env.sml";
use "../utils/Utils.sml";

structure ValexEnvInterp = struct

open Valex

(* val run : Valex.pgm -> int list -> valu *)
fun run (Valex(fmls,body)) ints = 
  let val flen = length fmls
      val ilen = length ints 
  in 
      if flen = ilen then 
          eval body (Env.make fmls (map (fn i => Int i) ints))
      else 
          raise (EvalError ("Program expected " ^ (Int.toString flen)
                            ^ " arguments but got " ^ (Int.toString ilen)))
  end
      
(* val eval : Valex.exp -> valu Env.env -> valu *)
and eval (Lit v) env = v
  | eval (Var name) env = 
    (case Env.lookup name env of
	 SOME(v) => v
       | NONE => raise (EvalError("Unbound variable: " ^ name)))
  | eval (PrimApp(primop, rands)) env =
    (primopFunction primop) (map (Utils.flip2 eval env) rands)
  | eval (Bind(name,defn,body)) env =
    eval body (Env.bind name (eval defn env) env)
  | eval (If(tst,thn,els)) env =
    (case eval tst env of
	 Bool false => eval els env
      | _  => eval thn env

(*	 
	       Bool b => if b then eval thn env else eval els env
	| v => raise (EvalError ("Non-boolean test value " 
				 ^ (valueToString v)
				 ^ " in if expression"))*)
    )

(* A function for running programs expressed as strings *)
fun runString pgmString args = 
  run (sexpToPgm (Sexp.stringToSexp pgmString)) args	  

(* A function for running a programs in a files *)
fun runFile pgmFile args = 
  run (sexpToPgm (Sexp.stringToSexp (Utils.fileToString pgmFile))) args	  

(* An interactive read-eval-print loop (REPL) for Sigmex expressions.
   By default, assumes zero arguments, but this can be changed
   with the #args directive (see below). The following directives
   are supported:

   + (#desugar exp) prints out the desugared form of exp

   + (#args (a_1 i_1) ... (a_n i_n)):  Install the n integers i_ 1 ... i_n 
     as the current program arguments a_1 ... a_n

     + (#runFile <pgm> <arg1> ... <arg_n>) runs the program specified
       by <filename> on the arguments in <args>, where

       - <pgm> is a symbol or string naming a file containing the program
         or an sexp representation of the program.

       - <arg_i> are integer program arguments

       E.g., (#runFile squares.vlx 7 5)
             (#runFile "squares.vlx" 7 5)
             (#runFile (valex (x y) (/ (+ x y) 2)) 5 15)

   + (#quit): Exit the interpreter
 *)

(* val repl : unit -> unit *)
fun repl () =
  (* sexpToStringIntPair : sexp -> (string * int) *)
  let fun println s = print (s^"\n")
      (* sexpToInt : sexp -> int *)
      fun sexpToInt (Sexp.Int i) = i
	| sexpToInt sexp = raise (Fail ("Not an int!: " ^ (Sexp.sexpToString sexp)))
      (* sexpToStringIntPair : sexp -> (string * int) *)
      fun sexpToSymIntPair (Sexp.Seq [Sexp.Sym s, Sexp.Int i]) = (s, i)
	| sexpToSymIntPair sexp = raise (Fail ("Not an symbol/int pair!"
					       ^ (Sexp.sexpToString sexp)))
      (* get a Valex program from a specification *)
      (* treat a symbol or string as the name of a file containing the program *)
      fun getPgm (Sexp.Sym filename) = fileToPgm filename
	| getPgm (Sexp.Str filename) = fileToPgm filename
	| getPgm sexp = sexpToPgm sexp 

      fun loop env = 
	let val _ = print "\nvalex> "
	    val sexp = Sexp.readSexp ()
	in case sexp of
	       Sexp.Seq [Sexp.Sym "#quit"] => println "Moriturus te saluto!"
	     | Sexp.Seq [Sexp.Sym "#desugar", sexp] =>
	       (println (expToString (sexpToExp sexp)); 
		(* sexpToExp performs both the desugaring of desugar and the bindpar desugaring *)
		loop env)
             | Sexp.Seq ((Sexp.Sym "#args") :: bindingxs) => 
	       let val (names, ints) = ListPair.unzip (map sexpToSymIntPair bindingxs)
	       in 
		   loop (Env.make names (map (fn i => Int i) ints))
	       end
	     | Sexp.Seq ((Sexp.Sym "#run") :: pgmx :: intxs) => 
	       let val _ = println (valueToString (run (getPgm pgmx) (List.map sexpToInt intxs)))
			   handle EvalError s => println ("Error: " ^ s)
				| SyntaxError s => println ("Error: " ^ s)
				| Fail s => println ("Error: " ^ s)
				| other => println ("Error: " ^ (exnName other)
						    ^ " -- " ^ (exnMessage other))
	       in loop env
	       end
		   
	     | _ => let val  _ = println (valueToString (eval (sexpToExp sexp) env))
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
