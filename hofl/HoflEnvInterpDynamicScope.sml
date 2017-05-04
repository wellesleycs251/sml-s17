(* Dynamically-scoped environment model interpreter for Hofl *)

use "../hofl/Hofl.sml";

structure HoflEnvInterpDynamicScope = struct

open Hofl

(* val run : Hofl.pgm -> int list -> valu *)
fun run (Hofl(fmls,body)) ints = 
  let val flen = length fmls
      val ilen = length ints 
  in 
      if flen = ilen then 
          eval body (Env.make fmls (map (fn i => Int i) ints))
      else 
          raise (EvalError ("Program expected " ^ (Int.toString flen)
                            ^ " arguments but got " ^ (Int.toString ilen)))
  end
      
(* val eval : Hofl.exp -> valu Env.env -> valu *)
and eval (Lit v) env = v
  | eval (Var name) env = 
    (case Env.lookup name env of
	 SOME(v) => v
       | NONE => raise (EvalError("Unbound variable: " ^ name)))
  | eval (PrimApp(primop, rands)) env =
    (primopFunction primop) (map (Utils.flip2 eval env) rands)
  | eval (If(tst,thn,els)) env =
    (case eval tst env of
	       Bool b => if b then eval thn env else eval els env
	| v => raise (EvalError ("Non-boolean test value " 
				 ^ (valueToString v)
				 ^ " in if expression"))
    )
  | eval (Abs(fml,body)) env = Fun(fml,body,env) (* make a closure *)
  | eval (App(rator,rand)) env = apply (eval rator env)
				       (eval rand env)
				       env
    (* dynamically scoped interpreter passes env 
       as an extra argument to apply *)
  | eval (Bindrec(names,defns,body)) env =
    eval body
         (Env.fix (* magic to peform knot-tying for recursive definitions *)
	      (fn e =>
                  (Env.bindAllThunks names 
                                     (map (fn defn => (fn () => eval defn e))
                                          defns)
                                     env)))

and apply (Fun(fml,body,staticEnv)) arg dynamicEnv =
    (* dynamically scoped interpreter accepts 
       dynamicEnv as an extra argument to apply *)
    eval body (Env.bind fml arg dynamicEnv)
  (* dynamically scoped interpreter extends dynamic environment at point 
     of call rather than static env at point of closure creation *)
  | apply fcn arg denv = raise (EvalError ("Non-function rator in application: "
					   ^ (valueToString fcn)))
	 
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

   + (#desugar <exp-or-decl>) prints out the desugared form of <exp-or-decl>

   + (def <name> <exp>) introduces a top-level binding of <name> to <exp>. 
     This binding is mutually recursive with all other top-level bindings.

   + (def (<name> <fml1> ... <fmln>) <exp>) is sugar for
     (def <name> (fun (<fml1> ... <fmln>) <exp>))

   + (load <filename>) loads definitions and other recursive loads
     from file named <filename> (which must be a string literal). 

   + (#runFile <pgm> <arg1> ... <arg_n>) runs the program specified
     by <filename> on the arguments in <args>, where

     - <pgm> is a symbol or string naming a file containing the program
       or an sexp representation of the program.

     - <arg_i> are integer program arguments

       E.g., (#runFile squares.vlx 7 5)
             (#runFile "squares.vlx" 7 5)
             (#runFile (hofl (x y) (/ (+ x y) 2)) 5 15)

   

   + (#quit): Exit the interpreter
 *)

(* val repl : unit -> unit *)
fun repl () =
  (* Repl loop carries with it a list of name/exp bindings introduced by declarations
     of the form (def ...) and (load ...). 
     These are used as the bindings of a BINDREC every time an expression is 
     evaluated. Thus, all the bindings are mutually recursive. *)
  let fun println s = print (s^"\n")
      (* sexpToInt : sexp -> int *)
      fun sexpToInt (Sexp.Int i) = i
	| sexpToInt sexp = raise (Fail ("Not an int!: " ^ (Sexp.sexpToString sexp)))
      (* sexpToStringIntPair : sexp -> (string * int) *)
      fun sexpToSymIntPair (Sexp.Seq [Sexp.Sym s, Sexp.Int i]) = (s, i)
	| sexpToSymIntPair sexp = raise (Fail ("Not an symbol/int pair!"
					       ^ (Sexp.sexpToString sexp)))
      (* get a Hofl program from a specification *)
      (* treat a symbol or string as the name of a file containing the program *)
      fun getPgm (Sexp.Sym filename) = fileToPgm filename
	| getPgm (Sexp.Str filename) = fileToPgm filename
	| getPgm sexp = sexpToPgm sexp 

      fun loop bindings = 
	let val _ = print "\nhofl-dynamic-scope> "
	    val sexp = Sexp.readSexp ()
	in case sexp of
	       Sexp.Seq [Sexp.Sym "#quit"] => println "Moriturus te saluto!"
	     | Sexp.Seq [Sexp.Sym "#desugar", sexp] =>
	       let val _ = (case sexp of
				Sexp.Seq ((Sexp.Sym "def" | Sexp.Sym "load") :: _) =>
				List.app (fn (name,defnx) =>
					     println (Sexp.sexpToString
  							  (Sexp.Seq [Sexp.Sym "def",
								     Sexp.Sym name,
								     desugar defnx])))
					 (declToBindings sexp)
 			      | _ => print (Sexp.sexpToString (desugar sexp)))
			   handle SyntaxError s => println ("SyntaxError: " ^ s)
				| Sexp.IllFormedSexp s => (println ("SexpError: " ^ s))
				| Fail s => println ("Failure: " ^ s)
				| other => println ("Error: " ^ (exnName other)
						    ^ " -- " ^ (exnMessage other))
	       in loop bindings
	       end
	     | Sexp.Seq ((Sexp.Sym "def" | Sexp.Sym "load") :: _) => 
	      let val newBindings = map (fn (name,defnx) => (name, sexpToExp defnx))
  	 				(declToBindings sexp)
				    handle SyntaxError s => (println ("SyntaxError: " ^ s); [])
					 | Sexp.IllFormedSexp s => (println ("SexpError: " ^ s); [])
					 | Fail s => (println ("Failure: " ^ s); [])
					 | other => (println ("Error: " ^ (exnName other)
							      ^ " -- " ^ (exnMessage other)); [])
		  val _ = List.app (fn (name,_) => println name) newBindings
				   (* print each declaration name *)
				   (* Add each binding to end of current list of bindings *)
	      in loop (bindings @ newBindings)
	      end
	(*
	      handle SyntaxError s => (println ("SyntaxError: " ^ s); loop bindings)
		   | Sexp.IllFormedSexp s => (println ("SexpError: " ^ s); loop bindings)
		   | Fail s => (println ("Failure: " ^ s); loop bindings)
		   | other => (println ("Error: " ^ (exnName other)
				       ^ " -- " ^ (exnMessage other)); loop bindings)
          *)
	     | Sexp.Seq ((Sexp.Sym "#run") :: pgmx :: intxs) => 
	       let val _ = println (valueToString (run (getPgm pgmx) (List.map sexpToInt intxs)))
			   handle EvalError s => println ("EvalError: " ^ s)
				| SyntaxError s => println ("SyntaxError: " ^ s)
				| Sexp.IllFormedSexp s => (println ("SexpError: " ^ s))
				| Fail s => println ("Failure: " ^ s)
				| other => println ("Error: " ^ (exnName other)
						    ^ " -- " ^ (exnMessage other))
	       in loop bindings
	       end
	     | _ => 
               (* (* Bindseq-based interpretation of top-level definitions. *)
	      let val (names,defns) = ListPair.unzip bindings 
                   val _ = println 
		       (valueToString 
		        (eval (sexpToExp sexp)
			      (ListUtils.foldr2 
                                 (* Since more recent bindings are at front of list, 
                                    foldr2 defines a name to most recent definition. *)
                                 (fun n d e -> Env.bind n (eval d e) e)
                                 Env.empty
                                 names
                                 defns))) 
                 in loop bindings
                end
            *)
           (* Bindrec-based interpretation of top-level definitions. *)
	      let val (names,defns) = ListPair.unzip (List.rev bindings) 
	          val _ = println (valueToString (eval (Bindrec(names,defns, sexpToExp sexp))
				    		      Env.empty))
			  handle EvalError s => println ("EvalError: " ^ s)
			       | SyntaxError s => println ("SyntaxError: " ^ s)
			       | Sexp.IllFormedSexp s => (println ("SexpError: " ^ s))
			       | Fail s => println ("Failure: " ^ s)
			       | other => println ("Error: " ^ (exnName other)
						   ^ " -- " ^ (exnMessage other))
              (* although bindings are mutually recursive, pay attention
                   to more recent bindings when the same name is defined twice. *)
	      in loop bindings
	      end
	end
  in loop []
  end
end
