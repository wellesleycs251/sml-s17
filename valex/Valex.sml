(***************************************************************
 Valex adds the following to Bindex: 
 * there are no longer only integer values but also  
   boolean, character, string, symbol, and list values 
 * conditionals 
 * a library of primitive operators
 * generalized primitive application with dynamic type checking
 * desugaring 
 ****************************************************************)

use "../utils/Env.sml";
use "../sexp/Sexp.sml";
use "../utils/Utils.sml";
use "../utils/StringSet.sml";

structure Valex = struct

  open Sexp			      

  exception SyntaxError of string
  exception Unbound of string list 
  exception EvalError of string

  (************************************************************
   Abstract Syntax
   ************************************************************)

  type ident = string

  datatype pgm = Valex of ident list * exp (* param names, body *)

  and exp =
      Lit of value (* integer, boolean, character, string, symbol, and list literals *)
    | Var of ident (* variable reference *)
    | PrimApp of primop * exp list (* primitive application with rator, rands *)
    | Bind of ident * exp * exp (* bind name to value of defn in body *)
    | If of exp * exp * exp (* conditional with test, then, else *)

  and value = (* use value rather than val because val is an SML keyword *)
      Int of int 
    | Bool of bool
    | Char of char
    | String of string
    | Symbol of string
    | List of value list (* Recursively defined value *)

  and primop = Primop of ident * (value list -> value)
     (* Valex bakes the primop meaning function into the syntax! *)

  fun primopName (Primop(name,_)) = name
  fun primopFunction (Primop(_,fcn)) = fcn

  (* val valueToSexp : value -> sexp *)
  fun valueToSexp (Int i) = Sexp.Int i
    | valueToSexp (Bool b) = Sym (if b then "#t" else "#f")
    | valueToSexp (Char c) = Sexp.Chr c
    | valueToSexp (String s) = Str s
    | valueToSexp (Symbol s) = Seq [Sym "sym", Sym s]
    | valueToSexp (List []) = Sym "#e" (* special case *)
    | valueToSexp (List xs) = Seq (Sym "list" :: (map valueToSexp xs))

  (* val valueToString : value -> string *)
  fun valueToString (Int i) = if i < 0
			      then "-" ^ (Int.toString (0 - i)) (* use - rather than ~ ! *)
			      else Int.toString i
    | valueToString v = sexpToString (valueToSexp v)

  (* val valuesToString : value list -> string *)
  fun valuesToString vs = sexpToString (Seq (map valueToSexp vs))

  (************************************************************
   Dynamic type checking helper functions
   ************************************************************)

  fun checkInt (Int i) f = f i
    | checkInt v _ = raise (EvalError ("Expected an integer but got: " ^ (valueToString v)))

  fun checkBool (Bool b) f = f b
    | checkBool v _ = raise (EvalError ("Expected a boolean but got: " ^ (valueToString v)))

  fun checkChar (Char c) f = f c
    | checkChar v _ = raise (EvalError ("Expected a char but got: " ^ (valueToString v)))

  fun checkString (String s) f = f s
    | checkString v _ = raise (EvalError ("Expected a string but got: " ^ (valueToString v)))

  fun checkSymbol (Symbol s) f = f s
    | checkSymbol v _ = raise (EvalError ("Expected a symbol but got: " ^ (valueToString v)))

  fun checkList (List vs) f = f vs
    | checkList v _ = raise (EvalError ("Expected a list but got: " ^ (valueToString v)))

  fun checkAny v f = f v (* always succeeds *)

  fun checkZeroArgs f [] = f ()
    | checkZeroArgs f vs = raise (EvalError ("Expected zero arguments but got: " ^ (valuesToString vs)))

  fun checkOneArg check f [v] = check v f
    | checkOneArg _ f vs = raise (EvalError ("Expected one argument but got: " ^ (valuesToString vs)))

  fun checkTwoArgs (check1,check2) f [v1,v2] =
    check1 v1 (fn x1 => check2 v2 (fn x2 => f(x1,x2)))
    | checkTwoArgs _ _ vs = raise (EvalError ("Expected two arguments but got: " ^ (valuesToString vs)))


  fun arithop f = checkTwoArgs (checkInt,checkInt) (fn(i1,i2) => Int(f(i1, i2)))
  fun relop f = checkTwoArgs (checkInt,checkInt) (fn(i1,i2) => Bool(f(i1, i2)))
  fun logop f = checkTwoArgs (checkBool,checkBool) (fn(b1,b2) => Bool(f(b1, b2)))
  fun pred f = checkOneArg checkAny (fn v => Bool(f v))

  (************************************************************
   Library of primitive operators
   ************************************************************)				  
  val primops = [ 
    (* Arithmetic ops *)
    Primop("+", arithop op+),
    Primop("-", arithop op-),
    Primop("*", arithop op* ),
    Primop("/", arithop (fn(x,y) => 
                           if (y = 0) then 
                             raise (EvalError ("Division by 0: " 
					       ^ (Int.toString x)))
                           else x div y)), 
    Primop("%", arithop (fn(x,y) => 
                           if (y = 0) then 
                             raise (EvalError ("Remainder by 0: " 
					       ^ (Int.toString x)))
                           else x mod y)),
    Primop("^", arithop (fn(base,pow) => 
                           if pow < 0 then 
                             raise (EvalError ("Exponentiation by negative power: "
                                 ^ (Int.toString pow)))
                           else 
                               let fun loop n ans =
				     if n = 0 then ans else loop (n-1) (base*ans)
                               in loop pow 1
			       end)),
	
    (* Relational ops *)
    Primop("<", relop op<), 
    Primop("<=", relop op<=), 
    Primop("=", relop op=), 
    Primop("!=", relop op<>),
    Primop(">=", relop op>=), 
    Primop(">", relop op>), 

    (* Logical ops *)
    Primop("not", checkOneArg checkBool (fn b => Bool(not b))),
    Primop("and", logop (fn(a,b) => a andalso b)), (* *not* short-circuit! *)
    Primop("or", logop (fn(a,b) => a orelse b)), (* *not* short-circuit! *)
    Primop("bool=", logop op=),

    (* Char ops *)
    Primop("char=", checkTwoArgs (checkChar, checkChar) (fn(c1,c2) => Bool(c1=c2))),
    Primop("char<", checkTwoArgs (checkChar, checkChar) (fn(c1,c2) => Bool(c1<c2))),
    Primop("int->char", checkOneArg checkInt (fn i => Char(chr i))),
    Primop("char->int", checkOneArg checkChar (fn c => Int(ord c))),
    Primop("explode", checkOneArg checkString 
  	    (fn str => List (let fun loop i chars = 
				   if i < 0 then chars
				   else loop (i-1) ((Char (String.sub(str,i))) :: chars)
                             in loop ((String.size str)-1) []
			     end))),
    Primop("implode", checkOneArg checkList
            (fn chars => String (let fun recur [] = ""
                                       | recur ((Char c)::cs') = (String.str c)
								 ^ (recur cs')
				       | recur _ = raise (EvalError "Non-char in implode")
	                         in recur chars
				 end))),

    (* String ops *)
    Primop("str=", checkTwoArgs (checkString,checkString) (fn(s1,s2) => Bool(s1=s2))),
    Primop("str<", checkTwoArgs (checkString,checkString) (fn(s1,s2) => Bool(s1<s2))),
    Primop("strlen", checkOneArg checkString (fn s => Int(String.size s))),
    Primop("str+", checkTwoArgs (checkString,checkString) (fn(s1,s2) => String(s1^s2))),
    Primop("toString", checkOneArg checkAny (fn v => String(valueToString v))),

    (* Symbol op *)
    Primop("sym=", checkTwoArgs (checkSymbol,checkSymbol) (fn(s1,s2) => Bool(s1=s2))),
    Primop("sym->string", checkOneArg checkSymbol (fn s => String s)),
    Primop("string->sym", checkOneArg checkString (fn s => Symbol s)), 

    (* List ops *)
    Primop("prep", checkTwoArgs (checkAny,checkList) (fn(v,vs) => List (v::vs))),
    Primop("head", checkOneArg checkList 
			       (fn [] => raise (EvalError "Head of an empty list")
 			        | (v::_) => v)),
    Primop("tail", checkOneArg checkList
	            (fn [] => raise (EvalError "Tail of an empty list")
                     | (_::vs') => List vs')),
    Primop("empty?", checkOneArg checkList (fn vs => Bool(vs = []))), 
    Primop("empty", checkZeroArgs (fn () => List [])),
    Primop("nth", checkTwoArgs (checkInt,checkList)
	     (* In Valex nth, index arg comes *before* list arg *)
	     (fn(i,vs) => if (i < 1) orelse (i > (List.length vs)) 
	                  then raise (EvalError ("nth -- out-of-bounds index " ^ (Int.toString i)))
                          else List.nth(vs,i-1))), (* 1-based rather than 0-based indexing *)

    (* General Equality *)
    Primop("equal?", checkTwoArgs (checkAny, checkAny) (fn(v1,v2) => Bool(v1 = v2))),

    (* Errors *)
    Primop("error", checkTwoArgs (checkString,checkAny)
				 (fn(msg,value) => raise (EvalError ("Valex Error -- "
								    ^ msg ^ ": "
								    ^ (valueToString value))))), 

    (* Dynamic type predicates *)
    Primop("int?", pred (fn (Int _) => true | _ => false)),
    Primop("bool?", pred (fn (Bool _) => true | _ => false)),
    Primop("char?", pred (fn (Char _) => true | _ => false)),
    Primop("sym?", pred (fn (Symbol _) => true | _ => false)),
    Primop("string?", pred (fn (String _) => true | _ => false)),
    Primop("list?", pred (fn (List _) => true | _ => false))

  ] 

  val primopEnv = Env.make (map (fn (Primop(name,_)) => name) primops) primops

  fun isPrimop name = case Env.lookup name primopEnv of SOME _ => true | None => false

  fun findPrimop s = Env.lookup s primopEnv

  (************************************************************
   Desugaring (val desugar : sexp -> sexp)
  ************************************************************)

  (* Incremental rule-based desugaring *)				 
  fun desugar sexp = 
    let val sexp' = desugarRules sexp in 
	if Sexp.isEqual(sexp',sexp) 
        then case sexp of
		 Seq sexps => Seq (map desugar sexps)
               | _ => sexp
	else desugar sexp'
    end
	
  and desugarRules sexp = 
      case sexp of 

	(* Handle Intex arg refs as var refs *)
	  Seq [Sym "$", Sexp.Int i] => Sym ("$" ^ (Int.toString i)) 

	(* Note: the following desugarings for && and || allow 
           non-boolean expressions for second argument! *)
	| Seq [Sym "&&", x, y] => Seq [Sym "if", x, y, Sym "#f"]
	| Seq [Sym "||", x, y] => Seq [Sym "if", x, Sym "#t", y]

	(* Racket-style cond *)
	| Seq [Sym "cond", Seq [Sym "else", defaultx]] => defaultx
	| Seq (Sym "cond" :: Seq [testx, bodyx] :: clausexs) => 
          Seq [Sym "if", testx, bodyx, Seq(Sym "cond" :: clausexs)]

	| Seq [Sym "bindseq", Seq[], bodyx] => bodyx
	| Seq [Sym "bindseq", Seq ((Seq[Sym name,defnx])::bindingxs), bodyx]
          => Seq[Sym "bind", Sym name, defnx, Seq[Sym "bindseq", Seq bindingxs, bodyx]]

	(* Desugar (bindpar ((Id1 E1) ... (Idn En)) Ebody) 
           to (bind vals (list E1 ... En) (* vals a "fresh" name *)
                (bindseq ((Id1 (nth 1 vals)) ... (Idn (nth n vals)))
                  Ebody))
            *)
	| Seq [Sym "bindpar", Seq bindingxs, bodyx] =>
	  let val listVar = Utils.fresh "vals"
	      val (names, defnxs) = parseBindings bindingxs
	  in Seq[Sym "bind",
		 Sym listVar,
		 Seq (Sym "list" :: defnxs),
		 Seq [Sym "bindseq",
		      Seq (map (fn (name, index) =>
				   Seq[Sym name,
				       Seq [Sym "nth", Sexp.Int index, Sym listVar]])
			       (ListPair.zip(names, Utils.range 1 (1 + (length names))))),
		      bodyx]]
	  end
	(* list desugarings *)
	| Seq [Sym "list"] => Sym "#e"
	| Seq (Sym "list" :: headx :: tailx) => 
	  Seq [Sym "prep", headx, Seq (Sym "list" :: tailx)]

	(* Racket-like quotation *)
	| Seq [Sym "quote", Sexp.Int i] => Sexp.Int i (* These are sexps, not Valex values! *)
	| Seq [Sym "quote", Chr i] => Chr i 
	| Seq [Sym "quote", Str i] => Str i 
	(* Quoted special symbols denote themselves *)
	| Seq [Sym "quote", Sym "#t"] => Sym "#t"
	| Seq [Sym "quote", Sym "#f"] => Sym "#f"
	| Seq [Sym "quote", Sym "#e"] => Sym "#e"
	(* Other quoted symbols s denote (sym s) *)
	| Seq [Sym "quote", Sym s] => Seq [Sym "sym", Sym s]
	(* (quote (x1 ... xn)) => (list (quote x1) ... (quote xn)) *)
	| Seq [Sym "quote", Seq xs] => 
          Seq (Sym "list" :: (map (fn x => Seq[Sym "quote", x]) xs))

	| _ => sexp (* doesn't match a rule, so unchanged *)

  (* parse bindings of the form ((<name_1> <defnx_1>) ... (<name_n> <defnx_n>))
     into ([name_1,...,name_n], [defnx_1, ..., defnx_n]) *)
  and parseBindings bindingxs = 
      ListPair.unzip (map (fn (Seq[Sym name, defnx]) => (name, defnx)
  		           | bindingx => raise (SyntaxError ("ill-formed bindpar binding"
							     ^ (sexpToString bindingx))))
			  bindingxs)

  (* For testing *)
  fun desugarString str = 
    Utils.println (sexpToString (desugar (stringToSexp str)))

  fun desugarFile filename = 
    Utils.println (sexpToString (desugar (fileToSexp filename)))

  (************************************************************
   Parsing from S-Expressions 
   ************************************************************)

  (* val sexpToPgm : Sexp.sexp -> pgm *)
  fun sexpToPgm (Seq [Sym "valex", Seq formals, bodyx]) =
      Valex(map symToString formals, sexpToExp bodyx)
    | sexpToPgm (Seq [Sym "bindex", Seq formals, bodyx]) =
      (* Handle Bindex programs as well *)
      Valex(map symToString formals, sexpToExp bodyx)
    | sexpToPgm (Seq [Sym "intex", Sexp.Int n, bodyx]) =
      (* Handle Intex programs as well *)
      Valex(map (fn i => "$" ^ (Int.toString i)) (Utils.range 1 n),
            sexpToExp bodyx)
    | sexpToPgm sexp = raise (SyntaxError ("invalid Valex program: " ^ (sexpToString sexp)))

  (* val symToString : Sexp.sexp -> string *)
  and symToString (Sym s) = s
    | symToString sexp = raise (SyntaxError ("symToString: not a string -- " ^ (sexpToString sexp)))

  and sexpToExp sexp = sexpToExp' (desugar sexp)

  (* val sexpToExp' : Sexp.sexp -> exp *)
  and sexpToExp' (Sexp.Int i) = Lit (Int i)
    | sexpToExp' (Sexp.Chr c) = Lit (Char c)
    | sexpToExp' (Sexp.Str s) = Lit (String s)
    (* Symbols beginning with # denote special values (not variables!) *)
    | sexpToExp' (Sym s) =
      if String.sub(s,0) = #"#"
      then Lit (stringToSpecialValue s)
      else Var s
    | sexpToExp' (Seq [Sym "sym", Sym s]) = Lit (Symbol s)
    | sexpToExp' (Seq [Sym "bind", Sym name, defnx, bodyx]) =
      Bind (name, sexpToExp' defnx, sexpToExp' bodyx)
    | sexpToExp' (Seq [Sym "if", testx, thenx, elsex]) =
      If(sexpToExp' testx, sexpToExp' thenx, sexpToExp' elsex)
    (* Clause for handling primops must come after all keywords *)
    | sexpToExp' (sexp as (Seq (Sym p :: randxs))) =
      (case findPrimop p of
	  SOME(prim) => PrimApp(prim, (* includes both name and primitive function! *)
				map sexpToExp' randxs)
       | NONE => raise (SyntaxError ("invalid Valex expression due to unknown primop "
				     ^ p ^ ": "
                                     ^ (sexpToString sexp))))
    | sexpToExp' sexp = raise (SyntaxError ("invalid Valex expression: "  
					    ^ (sexpToString sexp)))

  (* Strings beginning with # denote special values *)
  and stringToSpecialValue "#t" = Bool true   (* true and false are keywords *)
    | stringToSpecialValue "#f" = Bool false  (* for literals, not variables *)
    | stringToSpecialValue "#e" = List []     (* empty list literal *)
    | stringToSpecialValue str = raise (SyntaxError ("Unrecognized special value: " ^ str))

  (* val stringToExp : string -> exp *)
  and stringToExp s = sexpToExp (stringToSexp s) (* Desugar when possible *)

  (* val stringToPgm : string -> pgm *)
  and stringToPgm s = sexpToPgm (stringToSexp s)

  (* val fileToPgm : string -> pgm *)
  and fileToPgm filename = sexpToPgm (fileToSexp filename)

  (************************************************************
   Unparsing to S-Expressions
   ************************************************************)

  (* val pgmToSexp : pgm -> Sexp.sexp *)
  fun pgmToSexp (Valex(fmls, e)) =
    Seq [Sym "valex", Seq(map (fn s => Sym s) fmls), expToSexp e]

  (* val expToSexp : exp -> Sexp.sexp *)
  and expToSexp (Lit v) = valueToSexp v
    | expToSexp (Var s) = Sym s
    | expToSexp (PrimApp (rator, rands)) =
      Seq (Sym (primopName rator) :: map expToSexp rands)
    | expToSexp (Bind(name,defn,body)) =
      Seq [Sym "bind", Sym name, expToSexp defn, expToSexp body]
    | expToSexp (If(tst,thn,els)) =
      Seq [Sym "if", expToSexp tst, expToSexp thn, expToSexp els]

  (* val expToString : exp -> string *)
  and expToString s = sexpToString (expToSexp s)

  (* val pgmToString : pgm -> string *)
  and pgmToString s = sexpToString (pgmToSexp s)

  (************************************************************
   Free Variables 
  ************************************************************)

  structure S = StringSetList				   

  (* val freeVarsPgm : pgm -> S.t *)
  (* Returns the free variables of a program *)
  fun  freeVarsPgm (Valex(fmls,body)) = 
    S.difference (freeVarsExp body) (S.fromList fmls)

  (* val freeVarsExp : exp -> S.t *)
  (* Returns the free variables of an expression *)
  and freeVarsExp (Lit i) = S.empty
    | freeVarsExp (Var s) = S.singleton s
    | freeVarsExp (PrimApp(_,rands)) = freeVarsExps rands
    | freeVarsExp (Bind(name,defn,body)) =
		   S.union (freeVarsExp defn)
			   (S.difference (freeVarsExp body)
					 (S.singleton name))
    | freeVarsExp (If(tst,thn,els)) = freeVarsExps [tst,thn,els]

  (* val freeVarsExps : exp list -> S.t *)
  (* Returns the free variables of a list of expressions *)
  and freeVarsExps es = 
      foldr (fn (fvs, ans) => (S.union fvs ans)) S.empty (map freeVarsExp es)

  (* val varCheck : pgm -> unit *)
  and varCheck pgm = 
    let val unbounds = freeVarsPgm pgm 
    in if S.isEmpty unbounds then
           () (* OK *)
       else 
           raise (Unbound (S.toList unbounds))
    end

end 




