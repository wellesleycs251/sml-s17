use "../sexp/Sexp.sml";

structure Intex =

struct

datatype pgm = Intex of int * exp
     and exp = Int of int
             | Arg of int
	     | BinApp of binop * exp * exp
     and binop = Add | Sub | Mul | Div | Rem

val sqr = Intex(1, BinApp(Mul, Arg 1, Arg 1))
val avg = Intex(2, BinApp(Div, BinApp(Add, Arg 1, Arg 2), Int 2))
val f2c = Intex(1, BinApp(Div,
			  BinApp(Mul,
				 BinApp(Sub, Arg 1, Int 32),
				 Int 5),
			  Int 9))
val divRem = Intex(5, BinApp(Add,
			     BinApp(Mul,
				    BinApp(Div, Arg 1, Arg 2),
				    Arg 3),
			     BinApp(Rem, Arg 4, Arg 5)))

exception EvalError of string

fun run (Intex(numargs, exp)) args =
  if numargs <> length args
  then raise EvalError "Mismatch between expected and actual number of args"
  else eval exp args

and eval (Int i) args = i
  | eval (Arg index) args = 
    if (index <= 0) orelse (index > length args)
    then raise EvalError "Arg index out of bounds"
    else List.nth(args, index-1)
  | eval (BinApp(binop, exp1, exp2)) args =
    let val i1 = eval exp1 args
	val i2 = eval exp2 args
    in (case (binop, i2) of
	    (Div, 0) => raise EvalError "Division by 0"
	  | (Rem,0) => raise EvalError "Remainder by 0"
	  | _ => (binopToFun binop)(i1, i2))
    end

and binopToFun Add = op+
  | binopToFun Mul = op*
  | binopToFun Sub = op-
  | binopToFun Div = (fn(x,y) => x div y)
  | binopToFun Rem = (fn(x,y) => x mod y)

(* Parsing from S-Expressions *)

exception SyntaxError of string

fun sexpToPgm (Sexp.Seq[Sexp.Sym "intex", Sexp.Int n, body]) =
    Intex(n, sexpToExp body)
  | sexpToPgm sexp = raise (SyntaxError ("invalid Intex program: "
					 ^ (Sexp.sexpToString sexp)))

and sexpToExp (Sexp.Int i) = Int i
  | sexpToExp (Sexp.Seq[Sexp.Sym "$", Sexp.Int i]) = Arg i
  | sexpToExp (Sexp.Seq[Sexp.Sym p, rand1, rand2]) =
    BinApp(stringToPrimop p, sexpToExp rand1, sexpToExp rand2)
  | sexpToExp sexp =  raise (SyntaxError ("invalid Intex expression: "
					  ^ (Sexp.sexpToString sexp)))

and stringToPrimop "+" = Add
  | stringToPrimop "-" = Sub
  | stringToPrimop "*" = Mul
  | stringToPrimop "/" = Div
  | stringToPrimop "%" = Rem
  | stringToPrimop s = raise (SyntaxError ("invalid Intex primop: " ^ s))

(* val stringToExp : string -> exp *)			     
and stringToExp s = sexpToExp (Sexp.stringToSexp s)

(* val stringToPgm : string -> pgm *)			      
and stringToPgm s = sexpToPgm (Sexp.stringToSexp s)

(* val fileToPgm : string -> pgm *)
and fileToPgm filename = sexpToPgm (Sexp.fileToSexp filename)			      

(* Unparsing from S-Expressions *)

fun pgmToSexp (Intex(n,body)) =
  Sexp.Seq[Sexp.Sym "intex", Sexp.Int n, expToSexp body]

and expToSexp (Int i) = Sexp.Int i
  | expToSexp (Arg i) = Sexp.Seq[Sexp.Sym "$", Sexp.Int i]
  | expToSexp (BinApp (rator, rand1, rand2)) =
    Sexp.Seq[Sexp.Sym (primopToString rator),
	     expToSexp rand1, expToSexp rand2]

and primopToString Add = "+"
    | primopToString Sub = "-"
    | primopToString Mul = "*"
    | primopToString Div = "/"
    | primopToString Rem = "%"

and expToString s = Sexp.sexpToString (expToSexp s)
and pgmToString s = Sexp.sexpToString (pgmToSexp s)			      

end

(* Test cases *)

open Intex

val sqrTest = run sqr [5]
val avgTest = run avg [5,15]
val f2cTest = run f2c [86]

fun testRun pgm args =
  Int.toString (run pgm args) (* Convert to string so same type as error messages below *)
  handle EvalError msg => "EvalError: " ^ msg
       | other => "Unknown exception: " ^ (exnMessage other)

exception SexpError of string * Sexp.sexp					      
(* testRun' takes sexpStrings instead *)
fun testRun' pgmSexpString argsSexpString =				     
    testRun (stringToPgm pgmSexpString)
	    (sexpStringToIntList argsSexpString)
    handle SexpError (msg, sexp) => ("SexpError: " ^ msg ^ " " ^ (Sexp.sexpToString sexp))
         | Sexp.IllFormedSexp msg => ("SexpError: Ill-formed sexp " ^ msg)
         | other => "Unknown exception: " ^ (exnMessage other)


						

and sexpStringToIntList str =
    let val sexp = Sexp.stringToSexp str
    in case sexp of
	   Sexp.Seq xs => map sexpToInt xs
	 | _  => raise SexpError("expected sexp sequence but got", sexp)
    end

and sexpToInt (Sexp.Int i) = i
  | sexpToInt sexp = raise SexpError("expected sexp int but got", sexp)


val avgTest2 = testRun' "(intex 2 (/ (+ ($ 1) ($ 2)) 2))" "(5 15)"
val f2cTest2 = map (testRun' "(intex 1 (/ (* (- ($ 1) 32) 5) 9))")
		   ["(-40)", "(0)", "(32)", "(98)", "(212)"]
			   

  (* An interactive read-eval-print loop (REPL) for Intex expressions.
     By default, assumes zero arguments, but this can be changed
     with the #args directive (see below). The following directives
     are supported:

     + (#args i_1 i_n): Installs the n integers i_ 1 ... i_n 
       as the current positional program arguments

     + (#run <pgm> <arg1> ... <arg_n>) runs the program specified
       by <filename> on the arguments in <args>, where

       - <pgm> is a symbol or string naming a file containing the program
         or an sexp representation of the program.

       - <arg_i> are integer program arguments

       E.g., (#run avg.itx 5 15)
             (#run "avg.itx" 5 15)
             (#run (intex 2 (/ (+ ($ 1) ($ 2)) 2)) 5 15)

     + (#quit): Exit the interpreter
 *)

  fun repl () =

    let

	fun println s = print (s^"\n")
		
	(* sexpToInt : sexp -> int *)
	fun sexpToInt (Sexp.Int i) = i
	  | sexpToInt sexp = raise (Fail ("Not an int!: " ^ (Sexp.sexpToString sexp)))
	(* getPgm : sexp -> pgm *)				    
	(* get a Bindex program from a specification *)
	(* treat a symbol or string as the name of a file containing the program *)
	fun getPgm (Sexp.Sym filename) = fileToPgm filename
	  | getPgm (Sexp.Str filename) = fileToPgm filename
	  | getPgm sexp = sexpToPgm sexp 

	fun loop ints = (* ints are positional arguments *)
	    let val _ = print "\nintex> " 
		val sexp = Sexp.readSexp()
	    in case sexp of 
		   Sexp.Seq [Sexp.Sym "#quit"] => println "Moriturus te saluto!"
		 | Sexp.Seq ((Sexp.Sym "#args") :: intxs) => 
		   let val args = map sexpToInt intxs
		   in loop args (* install args as new default arguments *)
		   end
		 | Sexp.Seq ((Sexp.Sym "#run") :: pgmx :: intxs) => 
		   let val _ = println (Int.toString (run (getPgm pgmx) (map sexpToInt intxs)))
			       handle EvalError s => println ("Error: " ^ s)
				    | SyntaxError s => println ("Error: " ^ s)
				    | Fail s => println ("Error: " ^ s)
				    | other => println ("Error: " ^ (exnName other)
							^ " -- " ^ (exnMessage other))
		   in loop ints (* use same default ints as before *)
		   end
		 (* Otherwise evaluate expression relative to current arguments
                    determined by #args *)      
		 | _ => let val  _ = println (Int.toString (eval (sexpToExp sexp) ints))
				     handle EvalError s => println ("Error: " ^ s)
					  | SyntaxError s => println ("Error: " ^ s)
					  | Fail s => println ("Error: " ^ s)
					  | other => println ("Error: " ^ (exnName other)
							      ^ " -- " ^ (exnMessage other))
			in loop ints (* use same default ints as before *)
			end
	    end
    in loop []
    end


	       
