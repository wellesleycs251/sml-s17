(***************************************************************
 Bindex adds names to Intex:
 + number of program args is replaced by a list of formal param names;
 + argument references are replaced by variable references;
 + a bind construct allows local names.

 Still, there is only one kind of value manipulated by the language: integers. 
****************************************************************)

use "../sexp/Sexp.sml";
use "../utils/Utils.sml";
use "../utils/StringSet.sml";

structure Bindex = struct

  open Sexp			      

  (************************************************************
   Abstract Syntax
   ************************************************************)
  type ident = string
  datatype pgm = Bindex of ident list * exp (* param names, body *)
       and exp = Int of int (* integer literal with value *)
	       | Var of ident (* variable reference *)
	       | BinApp of binop * exp * exp (* binary operator application with rator, rands *)
	       | Bind of ident * exp * exp (* bind name to value of defn in body *)
       and binop = Add | Sub | Mul | Div | Rem (* binary arithmetic ops *)

  exception SyntaxError of string
  exception Unbound of string list

  (************************************************************
   Parsing from S-Expressions 
  ************************************************************)

  (* val symToString : Sexp.sexp -> string *)
  fun symToString (Sym s) = s
    | symToString sexp = raise (SyntaxError ("symToString: not a string -- "
					     ^ (sexpToString sexp)))

  (* val sexpToPgm : Sexp.sexp -> pgm *)
  fun sexpToPgm (Seq [Sym "bindex", Seq formals, body]) =
      Bindex(map symToString formals, sexpToExp body)
    | sexpToPgm (Seq [Sym "intex", Sexp.Int n, body]) = 
      Bindex(map (fn i => "$" ^ (Int.toString i)) (Utils.range 1 (n+1)), 
		 sexpToExp body)
    | sexpToPgm sexp = raise (SyntaxError ("invalid Bindex program: "
					   ^ (sexpToString sexp)))

  (* val sexpToExp : Sexp.sexp -> exp *)
  and sexpToExp (Sexp.Int i) = Int i
    | sexpToExp (Sym s) = Var s
    (* Translate Intex arg references ($ n) as $n *)
    | sexpToExp (Seq [Sym "$", Sexp.Int n]) = Var ("$" ^ (Int.toString n))
    | sexpToExp (Seq [Sym p, rand1x, rand2x]) =
      BinApp(stringToBinop p, sexpToExp rand1x, sexpToExp rand2x)
    | sexpToExp (Seq [Sym "bind", Sym name, defnx, bodyx]) = 
      Bind (name, sexpToExp defnx, sexpToExp bodyx)
    | sexpToExp sexp = raise (SyntaxError ("invalid Bindex expression: "  
					   ^ (sexpToString sexp)))

  (* val stringToBinop : string -> binop *)
  and stringToBinop "+" = Add
    | stringToBinop "-" = Sub
    | stringToBinop "*" = Mul
    | stringToBinop "/" = Div
    | stringToBinop "%" = Rem
    | stringToBinop s = raise (SyntaxError ("invalid Bindex primop: " ^ s))

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
  fun pgmToSexp (Bindex(fmls, body)) = 
    Seq [Sym "bindex", Seq(map (fn s => Sym s) fmls), expToSexp body]

  (* val expToSexp : exp -> Sexp.sexp *)
  and expToSexp (Int i) = Sexp.Int i
    | expToSexp (Var s) = Sym s
    | expToSexp (BinApp (rator, rand1, rand2)) = 
      Seq [Sym (binopToString rator), expToSexp rand1, expToSexp rand2]
    | expToSexp (Bind(name, defn, body)) =
      Seq [Sym "bind", Sym name, expToSexp defn, expToSexp body]

  (* val binopToString : binop -> string *)
  and binopToString Add = "+"
    | binopToString Sub = "-"
    | binopToString Mul = "*"
    | binopToString Div = "/"
    | binopToString Rem = "%" 

  (* val expToString : exp -> string *)
  and expToString s = sexpToString (expToSexp s)

  (* val pgmToString : pgm -> string *)
  and pgmToString p = sexpToString (pgmToSexp p)

  (* val pgmToFile : pgm -> unit *)
  and pgmToFile p filename = Utils.stringToFile (pgmToString p) filename

  (************************************************************
   Free Variables (supersedes Static Arg Checking)
   ************************************************************)

  structure S = StringSetList

  (* val freeVarsPgm : pgm -> S.t *)
  (* Returns the free variables of a program *)
  fun freeVarsPgm (Bindex(fmls,body)) =
    S.difference (freeVarsExp body) (S.fromList fmls)
	       
  (* val freeVarsExp : exp -> S.t *)
  (* Returns the free variables of an expression *)
  and freeVarsExp (Int i) = S.empty
    | freeVarsExp (Var name) = S.singleton name
    | freeVarsExp (BinApp(_,rand1,rand2)) =
      S.union (freeVarsExp rand1) (freeVarsExp rand2)
    | freeVarsExp (Bind(name,defn,body)) = 
      S.union (freeVarsExp defn)
	      (S.difference (freeVarsExp body) (S.singleton name))

  (* val freeVarsExps : exp list -> S.t *)
  (* Returns the free variables of a list of expressions *)
  and freeVarsExps exps = foldr (fn (s1,s2) => S.union s1 s2)
				S.empty
				(map freeVarsExp exps)

  (* val varCheck : pgm -> bool *)
  and varCheck pgm = S.isEmpty (freeVarsPgm pgm)

end				
			      

		       
