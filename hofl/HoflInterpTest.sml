structure HoflTestEntries = struct

  open Hofl

  type result = 
      Val of Hofl.value
    | Err of string

  fun resultToString (Val v) = valueToString v
    | resultToString (Err s) = s

  val hoflEntries = 

   [
    ("inc", 
     "(hofl (a) ((abs x (+ x 1)) a))", 
     [([2], Val (Int 3)),
      ([3], Val (Int 4))]), 

    ("bind1", 
     "(hofl (a)
       (bindseq ((f (fun (w) w))
 	         (g (fun (x) (+ x 1))))
         (bindpar ((f (fun (y) (g ( * y 2))))
 	           (g (fun (z) (if (> z 1) z (f ( * z 3))))))
            (g a))))",
      [([1], Val (Int 3)),
       ([2], Val (Int 2))]),

    ("bind2", 
     "(hofl (a)
       (bindseq ((f (fun (w) w))
 	         (g (fun (x) (+ x 1))))
         (bindseq ((f (fun (y) (g ( * y 2))))
 	           (g (fun (z) (if (> z 1) z (f ( * z 3))))))
            (g a))))",
      [([1], Val (Int 7)),
       ([2], Val (Int 2))]),

    ("bind3", 
     "(hofl (a)
       (bindseq ((f (fun (w) w))
 	         (g (fun (x) (+ x 1))))
         (bindrec ((f (fun (y) (g ( * y 2))))
 	           (g (fun (z) (if (> z 1) z (f ( * z 3))))))
            (g a))))",
      [([1], Val (Int 6)),
       ([2], Val (Int 2))]),

    ("scope1", 
     "(hofl (a b)
        (bind f (fun (w x) (list w x a b))
          (bind a (* b b)
            (bind g (fun (y z) (list y z a b))
              (bind b (+ a 1)
                (list (f a b) (g a b)))))))", 
      [([3,10], Val (List [List [Int 100, Int 101, Int 3, Int 10], 
                           List [Int 100, Int 101, Int 100, Int 10]]))]),

    ("fact", 
     "(hofl (x) (fact x)
        (def (fact n) 
          (if (= n 0)
              1
              (* n (fact (- n 1))))))",
     [([0], Val (Int 1)),
      ([1], Val (Int 1)),
      ([5], Val (Int 120)),]), 

    ("even/odd", 
     "(hofl (x) (list (even? x) (odd? x))
        (def (even? n)
          (if (= n 0)
              #t
              (odd? (- n 1))))
        (def (odd? n)
          (if (= n 0)
              #f
              (even? (- n 1)))))", 
     [([0], Val (List [Bool true, Bool false])), 
      ([1], Val (List [Bool false, Bool true])), 
      ([100], Val (List [Bool true, Bool false])), 
      ([101], Val (List [Bool false, Bool true])), 
      ]),

    ("range",
     "(hofl (x y) (range x y)
        (load \"list-utils.hfl\"))", 
     [([1,4], Val (List [Int 1, Int 2, Int 3, Int 4])),
      ([3,7], Val (List [Int 3, Int 4, Int 5, Int 6, Int 7])), 
      ([17,17], Val (List [Int 17])), 
      ([17,16], Val (List [])), 
      ]),

    ("rev",
     "(hofl (x) (rev (range 1 x))
        (load \"list-utils.hfl\"))", 
     [([4], Val (List [Int 4, Int 3, Int 2, Int 1])),
      ([1], Val (List [Int 1])), 
      ([0], Val (List [])), 
      ]),

    ("env",
     "(hofl () 
        (bindseq ((e0 env-empty)
                  (e1 (env-bind (sym a) 1 e0))
                  (e2 (env-bind (sym b) 2 e1))
                  (e3 (env-bind (sym a) 3 e2))
                  (envs (list e0 e1 e2 e3)))
          (list (map (fun (e) (env-lookup (sym a) e)) envs)
                (map (fun (e) (env-lookup (sym b) e)) envs)
                (map (fun (e) (env-lookup (sym c) e)) envs)))
        (load \"env.hfl\"))", 
     [([], Val (List [List [Symbol "*none*", Int 1, Int 1, Int 3], 
                      List [Symbol "*none*", Symbol "*none*", Int 2, Int 2], 
                      List [Symbol "*none*", Symbol "*none*", Symbol "*none*", Symbol "*none*"]]))
      ]),

   ]

  (* Translate Valex entries of the form (<ints>, ValexTestEntries.Val (Valex.Int <int>))
     to the form (<ints>, HoflTestEntries.Val (Hofl.Int <int>))
     and those of the form (<ints>, ValexTestEntries.Err <string>)
     to the form (<ints>, HoflTestEntries.Err <string>) *)

  fun translateValexEntries entries = 
    map (fn (name,pgmString,inouts) => 
          (name,pgmString,
	   map (fn (args,ans) => 
                  (case ans of
                     ValexTestEntries.Val v -> (args, Val (valexToHoflVal v))
                   | ValexTestEntries.Err s -> (args, Err s)))
                inouts))
	 entries
 
  and valexToHoflVal v = 
    case v of
      Valex.Int i => Hofl.Int i
    | Valex.Bool b => Hofl.Bool b
    | Valex.Char c => Hofl.Char c
    | Valex.String s => Hofl.String s
    | Valex.Symbol s => Hofl.Symbol s
    | Valex.List xs => Hofl.List (map valexToHoflVal xs)

  val entries = (translateValexEntries ValexTestEntries.entries) @ hoflEntries

end

signature HOFL_INTERP = sig
  val run : Hofl.pgm -> int list -> Hofl.valu
end

signature HOFL_TEST = sig
  val test : unit -> unit
end

module HoflInterpTest (HoflInterp: HOFL_INTERP): HOFL_TEST  = struct

  open Hofl
  open HoflTestEntries
  open FunUtils
  open ListUtils

  module HoflTester = 
    MakeTester (
      struct
        type prog = string
        type arg = int
        type res = result

        let trial progString args = 
          try
            Val(HoflInterp.run (Hofl.stringToPgm progString) args)
          with 
            Hofl.EvalError(str) -> Err(str)         
          | Hofl.SyntaxError(str) -> Err(str)         
          | Sexp.IllFormedSexp(str) -> Err(str)         
            (* Other exceptions will be passed through by default *)
        let argToString = string_of_int
        let resEqual = (=)
        let resToString = resultToString

      end
    )

  let test () = HoflTester.testEntries HoflTestEntries.entries

end

