use "../utils/Utils.sml";

signature SEXP = sig 
    datatype sexp = Int of int
		  | Flt of real
		  | Str of string
		  | Chr of char
		  | Sym of string
		  | Seq of sexp list
    exception IllFormedSexp of string
    val isEqual : sexp * sexp -> bool				     
    val stringToSexp : string -> sexp
    val stringToSexps : string -> sexp list
    val fileToSexp : string -> sexp
    val fileToSexps : string -> sexp list
    val sexpToString : sexp -> string
    val sexpToString' : int -> sexp -> string
    val sexpsToString : sexp list -> string
    val sexpToFile : sexp -> string -> unit
    val readSexp : unit -> sexp

end

structure Sexp :> SEXP =

struct 

datatype sexp = Int of int
	      | Flt of real
	      | Str of string 
	      | Chr of char
	      | Sym of string 
	      | Seq of sexp list

  (************************************************************  	
   S-expression equality: not naturally an equality type 
   because of reals :-(
   ************************************************************)

  fun isEqual (Int i1, Int i2) = (i1 = i2)
    | isEqual (Flt r1, Flt r2) = Real.==(r1,r2) (* This is where equality breaks *)
    | isEqual (Str s1, Str s2) = (s1 = s2)
    | isEqual (Chr c1, Chr c2) = (c1 = c2)
    | isEqual (Sym s1, Sym s2) = (s1 = s2)
    | isEqual (Seq sexps1, Seq sexps2) =
      (length sexps1) = (length sexps2) andalso 
      List.all isEqual (ListPair.zip(sexps1,sexps2))
    | isEqual  _ = false (* sexps without the same tag aren't equal *)
			    
  (************************************************************
   Unparsing -- i.e, turning an s-expression into a string. 
   It is easy to produce a single-line string, but in practice
   is nicer to "pretty-print" the s-expression on several 
   lines when it doesn't fit the terminal width (typically
   80 characters). Below is a simple backtracking pretty-printing 
   algorithm; there are much fancier ones that do a better job
   and/or do not backtrack.
   ************************************************************)

  fun isSmallLen n = n <= 15
  fun isSmall s = isSmallLen (String.size s)

  fun sexpsToString sexps = 
    String.concatWith "\n\n" (List.map sexpToString sexps)

  and sexpToString sexp = sexpToString' 80 sexp

  and sexpToString' width sexp = String.concatWith "\n" (sexpToStrings width sexp)

  (* Unparse sexp as a list of pretty-printed lines *)
  (* When possible, try to have all lines be <= width chars wide *)
  and sexpToStrings width sexp = 
    case sexp of
      (* For leaf tokens (ints, floats, symbols, chars, strings)
         we "lose" if they're bigger than width. No way to address this. *)
      Int i => [Int.toString i]
    | Flt f => [Real.toString f]
    | Sym s => [s] 
    | Chr c => (* Don't introduce escape sequences *)
       ["'" ^ (String.str c) ^ "'"] 
    | Str s => (* Don't introduce escape sequences *)
       ["\"" ^ s ^ "\""] 
    | Seq [] => ["()"]
    | Seq (sexp1::sexps) => 
       (case sexpToStrings (width - 1) (* account for "(" *)
                           sexp1 of
          [s1] => (* First sexp fits on single line. 
                     Try to get format 
                       (s1 s2 s3 ... sn)
                     or 
                       (s1 s2 
                           s3
                           ... 
                           sn
                           )
                   *)
            squeeze width s1 sexps
        | strs => 
	    if (not (isSmallLen (width - 1))) andalso (List.all isSmall strs) then 
              (* It's annoying to print each small string on a separate line.
                 Instead print as shape 
                   (s1 s2 s3 ...
                    si ...
                    ...
                    ... sn)  *)
              (case stringArray strs (width - 1) of
		 [] => ["()"]
	       | str1 :: strs => 
		      ("(" ^ str1)
		      :: (List.map (prefix 1) strs)) (* stringArray adds ")" after final elt *)
	      else 
                   (* Resort to shape 
                       (s1 
                        s2 
                        s3 
                        ...
                        sn
                        ) 
                   *)
              seqToStrings 
		(strs @ (List.concat 
                             (List.map (sexpToStrings (width - 1)) sexps)))
       ) (* end outer case *)
   
   and squeeze width s1 sexps = 
     let val len1 = String.size s1 
         val rest1 = List.concat 
			 (List.map (sexpToStrings (width - len1 - 2))
				   sexps)
     in
       if (len1 + (totalLen rest1) + 3) <= width then 
         (* everything fits on one line. The "3" accounts
            for initial '(', the ' ' between s1 and rest1
            (if rest1 is non-empty) and the final ')' *)
         ["(" ^ (String.concatWith " " (s1::rest1)) ^ ")"]
       else          
           let val initial = len1 + 2 (* account for "(" and " ") *)
	   in
	       if (not (isSmallLen (width - initial))) andalso (isSmall s1) andalso (List.all isSmall rest1) then 
         (* It's annoying to print each small string on a separate line.
            Instead print as shape 
              (s1 s2 s3 ...
                  si ...
                  ...
                  ... sn)  *)
		   (case stringArray rest1 (width - initial) of
			  [] => ["(" ^ s1 ^ ")"]
		   | str1 :: strs => 
		     ("(" ^ s1 ^ " " ^ str1)
		     :: (List.map (prefix initial) strs)) (* stringArray adds ")" after final elt *)
               else if (initial + (maxLen rest1)) <= width then 
         (* everything fits into shape 
              (s1 s2 
                  s3 
                  ... 
                  sn
                  ) 
            *)
          (case rest1 of
             [] => ["(" ^ s1 ^ ")"]
           | (str1 :: strs) => 
                ("(" ^ s1 ^ " " ^ str1)
                :: (List.map (prefix initial) (* 1 for '(', 1 for ' ' *)  
                             (strs @ [")"])))
        else (* must resort to shape 
                  (s1 
                   s2 
                   s3 
                   ...
                   sn
                   ) 
                This requires backtracking on assumed width
              *)
          seqToStrings (s1 :: 
                       (List.concat 
                         (List.map (sexpToStrings (width - 1))
                                   sexps)))
	   end (* end inner let *)
     end (* end outer let *)

  and stringArray strs width = 
     (* arrange strings in the shape
           s1 s2 ....
           si ....
           ...
           ... sn  
       fitting the max number of strings on each line subject
       to constraint that no line is longer than width. 
       Assume that each string in strs has length <= width.
       stringArray also adds a close paren after final element. *)
    let fun loop rest line lineLen revLines = 
      case rest of
        [] => if lineLen + 1 <= width then 
	        List.rev ((line ^ ")") :: revLines)
	      else
	        List.rev (")" :: line :: revLines)
      |	s1::rest' => 
	  if lineLen = 0 then 
            (* we're starting a new line an s1 is assumed to have length <= width) *)
            loop rest' s1 (String.size s1) revLines
          else
              let val len1 = String.size s1
	      in 
		  if (lineLen + len1 + 1) <= width then (* the 1 is for space before s1 *)
		      loop rest' (line ^ " " ^ s1) (lineLen + len1 +1) revLines
		  else (* start a new line *)
		      loop rest' s1 len1 (line :: revLines)
	      end (* end inner let *)
    in loop strs "" 0 []
    end (* end outer let *)
   
  and seqToStrings strings = 
    case strings of
      [] => ["()"]
    | (fst :: rst) => 
        ("(" ^ fst) :: 
        ((List.map (fn s => " " ^ s) rst)
         @ [" )"])

  and prefix n str = (* prefix str with n spaces *)
    (Utils.stringMake n #" ") ^ str
    
  and totalLen strs = (* Total length of strings in list of strings *)
                      (* Count 1 for spaces between strings *)
    (List.length strs) - 1
    + (List.foldl
        (fn(str,n) => n + (String.size str))
        0
        strs)

  and maxLen strs = (* Length of longest string in list of strings *)
    List.foldl
     (fn(str,n) =>  Int.max(n, (String.size str)))
     0
     strs

  (************************************************************
   Scanning strings into s-expression tokens
   ************************************************************)

  datatype token = ATOM of string
		 | STRING of string
		 | CHAR of char
		 | LPAREN 
		 | RPAREN 

  fun tokenToString tok = 
    case tok of
      ATOM s => "{ATOM " ^ s ^ "}"
    | STRING s => "{STRING \"" ^ s ^ "\"}"
    | CHAR c => "{CHAR '" ^ (String.str c) ^ "'}"
    | LPAREN => "{LPAREN}"
    | RPAREN => "{RPAREN}"

  fun tokensToString toks = 
    case toks of
      [] => "[]"
    | [t] => "[" ^ (tokenToString t) ^ "]"
    | (t::ts) => "[" ^ (tokenToString t) 
                     ^ (List.foldr (fn(t, s) => (", " ^ (tokenToString t) ^ s)) "]" ts)

(*
(* [lyn, 8/9/08] Created these, but no longer needed. *)
  let tokenToShortString tok = 
    match tok with 
      ATOM s => s
    | STRING s => "\"" ^ s ^ "\""
    | CHAR c => "'" ^ (String.make 1 c) ^ "'"
    | LPAREN => "("
    | RPAREN => ")"

  let rec tokensToShortString toks = 
    match toks with 
      [] => ""
    | (LPAREN :: ts) => "(" ^ (tokensToShortString ts)
    | (RPAREN :: ((RPAREN :: _) as ts)) => ")" ^ (tokensToShortString ts)
    | (RPAREN :: ts) => ") " ^ (tokensToShortString ts)
    | (t :: ts) => (tokenToShortString t) ^ " " ^ (tokensToShortString ts)
*)

  exception IllFormedSexp of string

  (* Note: stringToTokens is a very compelling example of block structure --
     e.g. definining local functions inside another function definition. *)
  fun stringToTokens s = 
    let val len = String.size s 
	fun scanTokens i = 
	  if i >= len then 
              [] 
	  else (case String.sub(s,i) of
		   (#" " | #"\t" | #"\n" | #"\r" | #"\b") => scanTokens(i+1) (* ignore whitespace *)
		   |	#"(" => (LPAREN :: scanTokens(i+1))
		   |    #")" => (RPAREN :: scanTokens(i+1))
		   |	#"\"" => scanString (i+1) (i+1) [] (* start of string *)
		   |	#"'" => scanChar (i+1) (* start of char *)
		   |	#"{" => scanBlockComment (i+1) 0 (* beginning of block comment *)
 	           |	#";" => scanLineComment (i+1) (* beginning of line comment *)
                   | _ => scanSymbol i (i+1) (* get a symbol *)
					   )

	and scanString start k revChars = 
	    (* Look for end of strings quotes. Handle escapes along the way. 
              revChars is reversed list of chars seen so far *)
	    if k >= len then 
		raise (IllFormedSexp 
			   ("Sexp: input ended before end of string:\n"
			    ^ (String.implode(List.rev revChars))))
	    else 
		let val c = String.sub(s,k)
		in
		    if c = #"\"" then (* close double-quote ending string *)
			STRING(String.implode(List.rev revChars))::scanTokens(k+1)
		    else if c = #"\\" then (* begin escape sequence *)
			if (k+1) >= len then 
			    raise (IllFormedSexp 
				       ("Sexp: input ended before end of string:\n"
					^ (String.implode(List.rev (c::revChars)))))
			else 
			    scanString start (k+2) (escaped(String.sub(s,(k+1)))::revChars)
		    else (* continue reading string *)
			scanString start (k+1) (c::revChars)
		end
		    
	and escaped c = (* convert escaped char to special char *)
	    case c of
		#"t" => #"\t"
	      | #"n" => #"\n"
	      | #"r" => #"\r"
	      | #"b" => #"\b"
	      | #"\\" => #"\\"
	      | #"\"" => #"\""
	      | #"'" => #"'"
	      |  _ => raise (IllFormedSexp 
				 ("Sexp: unrecognized escape sequence:"
				  ^  "\\" ^ (String.str c)))

	and scanChar k = 
	    if k >= len then 
		raise (IllFormedSexp "Sexp: input ended before end of char literal") 
	    else 
		let val c = String.sub(s,k)
		in
		    if (c = #"\\") then (* begin escape sequence *)
			if k+2 >= len then 
			    raise (IllFormedSexp "Sexp: input ended before end of char literal")
			else if (String.sub(s,(k+2))) = #"'" then
			    CHAR(escaped(String.sub(s,(k+1))))::scanTokens(k+3)
			else
			    raise (IllFormedSexp 
				       ("Sexp: ill-formed char literal: "
					^ "'\\" ^ (String.substring(s,(k+1),2))))
		    else if k+1 >= len then 
			raise (IllFormedSexp "Sexp: input ended before end of char literal")
		    else if (String.sub(s,(k+1))) = #"'" then 
			CHAR(c)::scanTokens(k+2)
		    else 
			raise (IllFormedSexp 
				   ("Sexp: ill-formed char literal: "
				    ^ "'" ^ (String.substring(s,k,2))))
		end

	and scanLineComment k = 
	    if k >= len then 
		[]
	    else 
		let val c = String.sub(s, k)
		in if c = #"\n" then 
		       scanTokens (k+1)
		   else 
		       scanLineComment (k+1)
		end

	and scanBlockComment k nestLevel = 
	    (* Ignore characters in comments. Handle nesting levels
               appropriately to avoid summary execution by lyn. *)
	    if k >= len then 
		raise (IllFormedSexp 
			   "Sexp: input ended before end of block comment")
	    else 
		let val c = String.sub(s, k)
		in
		    if c = #"}" then 
			if nestLevel = 0 then 
			    scanTokens (k+1)
			else 
			    scanBlockComment (k+1) (nestLevel - 1)
		    else if c = #"{" then 
			scanBlockComment (k+1) (nestLevel + 1)
		    else 
			scanBlockComment (k+1) nestLevel
		end

	and scanSymbol start k = 
	    if k >= len then 
		[ATOM(String.substring(s,start,(k-start)))]
	    else let val c = String.sub(s, k)
		 in if List.exists (fn x => x = c)
				   [#" ", #"\t", #"\n", #"\r",
				    #"\b", #"(", #")", #"{"] then 
			ATOM(String.substring(s,start,(k-start)))::scanTokens(k)
		    else 
			scanSymbol start (k+1)
		 end
    in scanTokens 0
    end

  (************************************************************
   Parsing s-expression tokens into s-expresions
   ************************************************************)

  (* fromToks : token list => (sexp * token list) *)
  (* Consumes the tokens corresponding to the first s-expression,
     and returns this s-expression and the unprocessed tokens *)
  fun fromToks toks = 
    case toks of
      [] => raise (IllFormedSexp "Sexp: empty input!")
    | (STRING(s)::ts) => (Str(s), ts)
    | (CHAR(c)::ts) => (Chr(c), ts)
    | (ATOM(s)::ts) =>
      if String.isSubstring "." s
      then (case Real.fromString s of
		SOME f => (Flt(f), ts)
	      | NONE => (Sym(s), ts))
      else (case Int.fromString s of
		SOME i => (Int i, ts)
	      | NONE => (Sym(s), ts))
    | (LPAREN::ts) =>  
       (case fromToksList ts of
            (sexps,ts') => (Seq(sexps), ts'))
    | (RPAREN::_) =>  raise (IllFormedSexp "Sexp: unmatched right paren")

   (* fromToksList : token list => (sexp list * token list) *)
   (* Collects all sexps before next right paren *)
(*
   and fromToksList toks = 
     match toks with 
       [] => raise (IllFormedSexp "Sexp: empty input!")
     | (RPAREN::ts) => ([],ts)
     | _ => (match fromToks toks with 
               (sexp,ts) => 
                 (match fromToksList ts with 
                    (sexps,ts') => (sexp::sexps,ts')))
 *)

(* [lyn, 8/9/08] Can give better error messages with this version *)
  and fromToksList toks = 
      let fun process (revSexps, restToks) = 
	    (case restToks of
		[] => raise (IllFormedSexp ("Sexp: end of input before matching right paren -- ("
                                     ^ (String.concatWith " " (List.map sexpToString (List.rev revSexps)))))
	      | (RPAREN::ts) => (List.rev revSexps, ts)
	      | _ => let val (sexp, ts) = fromToks restToks in 
			 process (sexp :: revSexps, ts)
		     end)
      in process ([], toks)
      end

  fun stringToSexp s = 
      case fromToks (stringToTokens s) of
	  (sexp, []) => sexp
	| (sexp, toks) => 
	  raise (IllFormedSexp 
		     ("Sexp: extra tokens\n"
                      ^ (tokensToString toks)
                      ^ "\n\nafter end of sexp:\n\n"
                      ^ (sexpToString sexp)))

  fun fileToSexp filename = stringToSexp (Utils.fileToString filename)

  and stringToSexps s = toksToSexps (stringToTokens s)

  and toksToSexps toks =
    case toks of
	[] => []
      | _ => (case fromToks toks of
		  (sexp, toks') => sexp :: (toksToSexps toks'))

  and fileToSexps filename = 
      stringToSexps (Utils.fileToString filename)

  and sexpToFile sexp filename = 
      Utils.stringToFile (sexpToString sexp) filename

  (* Returns true if str is a single sexp and false otherwise *)
  and isSexp str = 
      let val _ = stringToSexp str in true end
      handle IllFormedSexp _ => false

  (* Read lines from standard input until have a complete s-expression *)
  and readSexp () = 
    let fun loop str = 
	  stringToSexp str
	  handle IllFormedSexp _ => loop (str ^ (Utils.readLineFromConsole()))
    in loop ""
    end

end
