(* Load PostFix and Bindex structures *)
use "../postfix/PostFix.sml"; (* semi-colon required! *)
use "../bindex/Bindex.sml"; (* semi-colon required! *)
use "../bindex/BindexEnvInterp.sml"; (* semi-colon required! *)
use "../utils/Env.sml"; (* semi-colon required! *)
use "../utils/Utils.sml"; (* semi-colon required! *)

(* Note that Bindex and PostFix structures overlap for constructors 
   Int, Add, Sub, Mul, Div, Rem and type pgm *)
structure BindexToPostFix = struct

fun makeArgEnv args = Env.make args (Utils.range 1 ((length args) + 1))
(* returned env associates each arg name with its 1-based index *)

fun envPushAll env = Env.map (fn index => index + 1) env
(* add one to the index for each name in env *)

fun envPush name env = Env.bind name 1 (envPushAll env)

fun bindexToPostFix (Bindex.Bindex(args, body)) =
  PostFix.PostFix(length args, expToCmds body (makeArgEnv args))

(* In expToCmds, env statically tracks the depth of each named variable
   value on the stack *) 
and expToCmds (Bindex.Int i) env = [PostFix.Int 42] (* replace this stub *)
  | expToCmds (Bindex.Var name) env = [PostFix.Int 42] (* replace this stub *)
  | expToCmds (Bindex.BinApp(binop, rand1, rand2)) env = [PostFix.Int 42] (* replace this stub *)
  | expToCmds (Bindex.Bind(name, defn, body)) env = [PostFix.Int 42] (* replace this stub *)
							
and binopToArithop Bindex.Add = PostFix.Add
  | binopToArithop Bindex.Sub = PostFix.Sub
  | binopToArithop Bindex.Mul = PostFix.Mul
  | binopToArithop Bindex.Div = PostFix.Div
  | binopToArithop Bindex.Rem = PostFix.Rem

(* Testing of bindex programs from files *)
fun testTranslator (bindexFileName, listOfArgLists) =
  let val _ = print ("-------------------------------------------------\n"
		     ^ "Testing Bindex program file "
		     ^ bindexFileName ^ "\n\n")
      val bindexPgm = Bindex.fileToPgm(bindexFileName)
      val _ = print ("Bindex program input to translation:\n"
		     ^ (Bindex.pgmToString bindexPgm) ^ "\n\n")
      val postfixPgm = bindexToPostFix(bindexPgm)
      val _ = print ("PostFix program output of translation:\n"
		     ^ (PostFix.pgmToString postfixPgm) ^ "\n\n")
      fun intsToString ints = "[" ^ (String.concatWith
					 ","
					 (map Int.toString ints)) ^ "]"
      fun testBehavior args =
	let val _ = print ("Testing args " ^ (intsToString args) ^ ": ")
	    val bindexResult = Int.toString (BindexEnvInterp.run bindexPgm args)
			       handle Bindex.SyntaxError msg =>
				        ("Syntax error: " ^ msg)
				      | BindexEnvInterp.EvalError msg =>
					("Eval error: " ^ msg)
				      | exn => (exnName exn) ^ ": "
					       ^ (exnMessage exn)
	    val postfixResult = Int.toString (PostFix.run postfixPgm args)
				handle PostFix.SyntaxError msg =>
				        ("Syntax error: " ^ msg)
				      | PostFix.ExecError msg =>
					("Eval error: " ^ msg)
				      | exn => (exnName exn) ^ ": "
					       ^ (exnMessage exn)
						    
	in if bindexResult = postfixResult
	   then print ("both programs return " ^ bindexResult ^ "\n\n")
	   else print ("*** ERROR IN TRANSLATION ***"
		       ^ "\n  Bindex result: " ^ bindexResult
		       ^ "\n  PostFix result: " ^ postfixResult
		       ^ "\n\n")
	end
  in List.app testBehavior listOfArgLists
  end

(* Testing *)
val tests =       
    List.app testTranslator
             [("../bindex/avg.bdx", [[3,7],[15,5]]), 
	      ("../bindex/avg2.bdx", [[3,7],[15,5]]),
	      ("../bindex/squares.bdx", [[5,3],[5,4],[10,9],[10,8],[10,7]]),
	      ("../bindex/quadratic.bdx", [[3,~7,2,0],[3,~7,2,1],[3,~7,2,2],[3,~7,2,3],[3,~7,2,4]]),
	      ("../bindex/non-commutative.bdx", [[7,2],[7,3],[7,4],[14,5]]),
	      ("../bindex/bind-operand.bdx", [[8,2],[12,3]]),
	      ("../bindex/scope.bdx", [[1,12],[2,12],[3,12],[4,12],[6,12]]),
   	      ("../bindex/scope2.bdx", [[1,12],[2,12],[3,12],[4,12],[6,12]])
	     ]
end

				
				
				
				
