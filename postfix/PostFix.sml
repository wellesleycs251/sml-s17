use "../sexp/Sexp.sml"; (* import Sexp module for use in PostFix *)

structure PostFix = struct

  (************************************************************
   Abstract Syntax
   ************************************************************)

  datatype pgm = PostFix of int * cmd list
       and cmd = Pop | Swap | Nget | Sel | Exec
		 | Int of int
		 | Seq of cmd list 
		 | Arithop of arithop
		 | Relop of relop
       and arithop = Add | Sub | Mul | Div | Rem
       and relop = Lt | Eq | Gt

  (*	   
   Sample program from postfix lecture
   (define pf1 '(postfix 2 2 nget 0 gt (sub) (swap 1 nget mul add) sel exec))	   
  *)

  val pf1 = PostFix(2, [Int 2, Nget, Int 0, Relop Gt,
			Seq[Arithop Sub],
			Seq[Swap, Int 1, Nget, Arithop Mul, Arithop Add],
   			Sel, Exec])
		   
  (************************************************************
   PostFix Interpreter
   ************************************************************)

  (* Stack values are either integers or executable sequences *)			       
  datatype stkval = IntVal of int | SeqVal of cmd list

						  
  exception ConfigError of string * cmd * stkval list
    (* for errors involving a bad stack for a command *)
  exception ExecError of string
    (* for other runtime errors during execution *)

  fun run (PostFix(numargs, cmds)) args =
    if numargs = List.length args
    then case execCmds cmds (map IntVal args) of
	     (IntVal v) :: _ => v
	   | _  => raise ExecError "Command sequence on top of final stack"
    else raise ExecError "Mismatch between expected and actual number of args"

  (* Perform all commands on given stack and return resulting stack *)
  and execCmds cmds vs = foldl (fn (cmd,stk) => execCmd cmd stk) vs cmds
						
  (* Perform command on given stack and return resulting stack *)	
  and execCmd (Int i) vs = (IntVal i) :: vs
    | execCmd (Seq cmds) vs = (SeqVal cmds) :: vs
    | execCmd Pop (v :: vs) = vs
    | execCmd Swap (v1 :: v2 :: vs) = v2 :: v1 :: vs
    | execCmd Nget (stk as (IntVal index) :: vs) =
      if index <= 0 orelse index > List.length(vs) then
	  raise ConfigError("Invalid index", Nget, stk)
      else
	  (case List.nth(vs, index-1) of
 	       (v as IntVal(_)) => v :: vs
             | SeqVal(_) => raise ConfigError("Nget can't get a command sequence",
					      Nget, stk))
    | execCmd Sel (v_else :: v_then :: (IntVal v_test) :: vs) =
      (if v_test = 0 then v_else else v_then) :: vs
    | execCmd Exec ((SeqVal cmds) :: vs) = execCmds cmds vs
    | execCmd (Arithop a) ((IntVal i1) :: (IntVal i2) :: vs)
      = (IntVal ((arithopToFun a)(i2, i1)) ) :: vs
    | execCmd (Relop r) ((IntVal i1) :: (IntVal i2) :: vs)
      = (IntVal (boolToInt( ((relopToFun r)(i2, i1)) ) ) ) :: vs
    | execCmd cmd stk = raise ConfigError("Illegal configuration", cmd, stk)

  and arithopToFun Add = op+
    | arithopToFun Mul = op*
    | arithopToFun Sub = op-
    | arithopToFun Div = (fn(x,y) => x div y)
    | arithopToFun Rem = (fn(x,y) => x mod y)

  and relopToFun Lt = op<
    | relopToFun Eq = op=
    | relopToFun Gt = op>

 and boolToInt false = 0
    | boolToInt true = 1

  (************************************************************
   Parsing from S-Expressions 
   ************************************************************)
			   
  exception SyntaxError of string

  fun sexpToPgm (Sexp.Seq(Sexp.Sym "postfix" :: Sexp.Int n :: cmdxs)) =
    PostFix(n, map sexpToCmd cmdxs)
    | sexpToPgm sexp = raise (SyntaxError ("invalid PostFix program: "
					   ^ (Sexp.sexpToString sexp)))
			     
  and sexpToCmd (Sexp.Int i) = Int i
    | sexpToCmd (Sexp.Seq cmdxs) = Seq (map sexpToCmd cmdxs)
    | sexpToCmd (Sexp.Sym "pop") = Pop
    | sexpToCmd (Sexp.Sym "swap") = Swap
    | sexpToCmd (Sexp.Sym "nget") = Nget
    | sexpToCmd (Sexp.Sym "sel") = Sel
    | sexpToCmd (Sexp.Sym "exec") = Exec
    | sexpToCmd (Sexp.Sym "add") = Arithop Add
    | sexpToCmd (Sexp.Sym "sub") = Arithop Sub
    | sexpToCmd (Sexp.Sym "mul") = Arithop Mul
    | sexpToCmd (Sexp.Sym "div") = Arithop Div
    | sexpToCmd (Sexp.Sym "rem") = Arithop Rem
    | sexpToCmd (Sexp.Sym "lt") = Relop Lt
    | sexpToCmd (Sexp.Sym "eq") = Relop Eq
    | sexpToCmd (Sexp.Sym "gt") = Relop Gt
    | sexpToCmd sexp = raise SyntaxError ("unknown command "
					  ^ (Sexp.sexpToString sexp))

  and stringToCmd s = sexpToCmd (Sexp.stringToSexp s)
  and stringToPgm s = sexpToPgm (Sexp.stringToSexp s)

  (************************************************************
   Unparsing to S-Expressions
   ************************************************************)

  fun pgmToSexp (PostFix(n,cmds)) =
    Sexp.Seq (Sexp.Sym "postfix" :: Sexp.Int n :: map cmdToSexp cmds)

  and cmdToSexp (Int i) = Sexp.Int i
    | cmdToSexp (Seq cmds) = Sexp.Seq (map cmdToSexp cmds)
    | cmdToSexp Pop = Sexp.Sym "pop"
    | cmdToSexp Swap = Sexp.Sym "swap"
    | cmdToSexp Nget = Sexp.Sym "nget"
    | cmdToSexp Sel = Sexp.Sym "sel"
    | cmdToSexp Exec = Sexp.Sym "exec"
    | cmdToSexp (Arithop Add) = Sexp.Sym "add"
    | cmdToSexp (Arithop Sub) = Sexp.Sym "sub"
    | cmdToSexp (Arithop Mul) = Sexp.Sym "mul"
    | cmdToSexp (Arithop Div) = Sexp.Sym "div"
    | cmdToSexp (Arithop Rem) = Sexp.Sym "rem"
    | cmdToSexp (Relop Lt) = Sexp.Sym "lt"
    | cmdToSexp (Relop Eq) = Sexp.Sym "eq"
    | cmdToSexp (Relop Gt) = Sexp.Sym "gt"

  and stkvalToSexp (IntVal i) = Sexp.Int i
    | stkvalToSexp (SeqVal cmds) = Sexp.Seq (map cmdToSexp cmds)

  and stkToSexp stkvals = Sexp.Seq (map stkvalToSexp stkvals)

  and cmdToString cmd = Sexp.sexpToString (cmdToSexp cmd)
  and pgmToString pgm = Sexp.sexpToString (pgmToSexp pgm)
  and stkvalToString v = Sexp.sexpToString (stkvalToSexp v)
  and stkToString stk = Sexp.sexpToString (stkToSexp stk)

end

(* Test cases *)    
open PostFix

fun testRun pgm args =
  Int.toString (run pgm args) (* Convert to string so same type as error messages below *)
  handle ExecError msg => "ExecError: " ^ msg
       | ConfigError(msg, cmd, stk) =>
	 "ConfigError: " ^ msg
	 ^ " command=" ^ (cmdToString cmd)
	 ^ " and stack=" ^ (stkToString stk)
       | General.Div => "Divide by zero error"
         (* General.Div from SML General basis structure;
            Need explicit qualification to distinguish from PostFix.Div *)
       | other => "Unknown exception: " ^ (exnMessage other)

(* test cases *)

val pfIntTest = testRun (PostFix(0, [Int 7, Int 8, Int 9])) []
val pfPopTest = testRun (PostFix(0, [Int 7, Int 8, Int 9, Pop])) []
val pfSwapTest = testRun (PostFix(0, [Int 7, Int 8, Int 9, Swap])) []
val pfNgetTest = testRun (PostFix(0, [Int 7, Int 8, Int 9, Int 3, Nget])) []
val pfAddTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Arithop(Add)])) []
val pfSubTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Arithop(Sub)])) []
val pfMulTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Arithop(Mul)])) []
val pfDivTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Arithop(Div)])) []
val pfRemTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Arithop(Rem)])) []
val pfLtTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Relop(Lt)])) []
val pfEqTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Relop(Eq)])) []
val pfGtTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Relop(Gt)])) []
val pfSelTest1 = testRun (PostFix(0, [Int 4, Int 7, Int 2, Sel])) []
val pfSelTest2 = testRun (PostFix(0, [Int 0, Int 7, Int 2, Sel])) []
val pfSeqTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Seq [Pop, Swap, Arithop(Sub)]])) []
val pfExecTest = testRun (PostFix(0, [Int 4, Int 7, Int 2, Seq [Pop, Swap, Arithop(Sub)], Exec])) []
val pfArgTest = testRun (PostFix(3, [Swap, Arithop(Div), Arithop(Sub)])) [7,2,1]
val pf1Tests = map (testRun pf1) [[3, 5], [3, ~5]]
(* expect ["2", "28"] *)

(* error tests *)		   
val pfArgMismatchTest = testRun (PostFix(3, [Swap, Arithop(Div), Arithop(Sub)])) [7,2]
val pfPopEmptyStack = testRun (PostFix(0, [Pop])) []
val pfSwapEmptyStack = testRun (PostFix(0, [Swap])) []
val pfSwapSingletonStack = testRun (PostFix(1, [Swap])) [4]
val pfArithopEmptyStack = testRun (PostFix(0, [Arithop Sub])) []
val pfArithopSingletonStack = testRun (PostFix(1, [Arithop Sub])) [4]
val pfRelopEmptyStack = testRun (PostFix(0, [Relop Lt])) []
val pfRelopSingletonStack = testRun (PostFix(1, [Relop Lt])) [4]
val pfNgetNegativeIndex = testRun (PostFix(3, [Int ~1, Nget])) [7, 9, 5]
val pfNgetZeroIndex = testRun (PostFix(3, [Int 0, Nget])) [7, 9, 5]
val pfNgetTooBigIndex = testRun (PostFix(3, [Int 4, Nget])) [7, 9, 5]
val pfNgetSeqVal = testRun (PostFix(3, [Seq[Swap,Pop], Int 1, Nget])) [7, 9, 5]

exception SexpError of string * Sexp.sexp

(* testRun' takes sexpStrings instead *)		   
fun testRun' pgmSexpString argsSexpString =
    testRun (stringToPgm pgmSexpString)
	    (sexpStringToIntList argsSexpString)
    handle SexpError (msg, sexp) => ("SexpError: " ^ msg ^ " " ^ (Sexp.sexpToString sexp))
         | Sexp.IllFormedSexp msg => ("SexpError: Ill-formed sexp " ^ msg)
         | SyntaxError msg => ("SyntaxError: " ^ msg)
         | other => "Unknown exception: " ^ (exnMessage other)

and sexpStringToIntList str =
    let val sexp = Sexp.stringToSexp str
    in case sexp of
	   Sexp.Seq xs => map sexpToInt xs
	 | _  => raise SexpError("expected sexp sequence but got", sexp)
    end

and sexpToInt (Sexp.Int i) = i
  | sexpToInt sexp = raise SexpError("expected sexp int but got", sexp)


val pf1String = "(postfix 2 2 nget 0 gt (sub) (swap 1 nget mul add) sel exec)"

val sosTest = testRun' "(postfix 2 1 nget mul swap 1 nget mul add)" "(3 4)"
		       
val pf1StringTests = map (testRun' pf1String) ["(3 5)", "(3 -5)"]		    
			   

		   


		   



	    
			      
