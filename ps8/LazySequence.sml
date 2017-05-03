use "../utils/Utils.sml"; (* For Utils.range *)

(* Put your name here:    *)

(* CS 251 Spring '17 Problem Set 8 Problem 1 LazySequences. 

   Complete the predicate-based representation of the SET signature.
   Represent a set as a membership predicate function. *)

signature SEQUENCE = sig

    (* The type of a sequence *)
    type ''a t 

    (* An empty sequence *)
    val empty : ''a t

    (* Create a sequence of (hi - lo) values fcn(lo), fcn(lo+1), ..., fcn(hi-1) *)
    val segment: int -> int -> (int -> ''a) -> ''a t

    (* Convert a lengnth-n list into a sequence of n values *)				
    val fromList: ''a list -> ''a t

    (* Concatenate two sequences: given	a length-m sequence s and length-n sequence t,
       returns a single length-m+n sequence that has all values of	s followed by all
       values of t *)
    val concat : ''a t -> ''a t -> ''a t

    (* Return the length of a sequence = number of values in it *)				
    val length : ''a t -> int

    (* Return the nth value of a sequence (0-indexed).
       Raise an IndexOutOfBounds exception for an out-of-bounds index. *)
    val get : int -> ''a t -> ''a

    (* Return a sequence that results from applying f to each elt *)
    val map : (''a -> ''b) -> ''a t -> ''b t

    (* Return a list with all elements in the sequence. The ith element of the resulting
       list should be the ith element of the given sequence. *)					   
    val toList : ''a t -> ''a list

end

exception Unimplemented (* Placeholder during development. *)
exception IndexOutOfBounds of int (* when use nth with out-of-bounds index *)

structure LazySequence :> SEQUENCE = struct

    type ''a thunkTy = unit -> ''a

    datatype ''a t = Segment of int * int * (int -> ''a) (* lo, hi, fcn *)
		   | ThunkList of ''a thunkTy list
		   | Concat of ''a t * ''a t

    fun segment lo hi fcn = Segment(lo,hi,fcn)
    fun fromList xs = ThunkList (List.map (fn x => (fn () => x)) xs)
    fun concat seq1 seq2 = Concat(seq1,seq2)
    fun dethunk thunk = thunk()				   

    val empty = ThunkList []

    fun length seq = 0 (* replace this stub *)

    fun get n seq = raise Unimplemented (* replace this stub *)
	    
    fun map f seq = empty (* replace this stub *)

    fun toList seq = [] (* replace this stub *)

end

(* Testing *)
open LazySequence

val _ = Control.Print.printLength := 1000


fun testGet seq =
  List.map (fn index => (index, get index seq)) (Utils.range 0 (length seq))
  handle Unimplemented => [] 

fun testGetRange seq lo hi =
  List.map (fn index => (index, get index seq)) (Utils.range lo hi)
  handle Unimplemented => [] 	   

fun testGetHandleException eltToString seq =
  let fun getHandleException (index) =
	eltToString(get index seq)
        handle  (IndexOutOfBounds n) => "Error: IndexOutOfBounds -- " ^ (Int.toString n)
	     |   exn => "Error: " ^ (exnName exn) ^ " -- " ^ (exnMessage exn)
  in List.map (fn index => (index, getHandleException(index))) (Utils.range 0 (length seq))
  end

fun testGetRangeHandleException eltToString seq lo hi =
  let fun getHandleException (index) =
	eltToString(get index seq)
        handle  (IndexOutOfBounds n) => "Error: IndexOutOfBounds -- " ^ (Int.toString n)
	     |   exn => "Error: " ^ (exnName exn) ^ " -- " ^ (exnMessage exn)		   
  in List.map (fn index => (index, getHandleException(index)))
	      (Utils.range lo hi)
  end      

val s1 = segment ~3 4 (fn n => n*n + 6*n)
val len_s1 = length s1
val testGet_s1 = testGet s1
val toList_s1 = toList s1
val testGetRangeHandleException_s1 =
    testGetRangeHandleException Int.toString s1 ~2 9

val map_dbl_s1 = map (fn n => n*2) s1
val len_map_dbl_s1 = length map_dbl_s1
val testGet_map_dbl_s1 = testGet map_dbl_s1
val toList_map_dbl_s1 = toList map_dbl_s1
val testGetRangeHandleException_map_dbl_s1 =
    testGetRangeHandleException Int.toString map_dbl_s1 ~2 9			     
				 
val map_even_s1 = map (fn n => n mod 2 = 0) s1
val len_map_even_s1 = length map_even_s1
val testGet_map_even_s1 = testGet map_even_s1
val toList_map_even_s1 = toList map_even_s1
val testGetRangeHandleException_map_dbl_map_even_s1 =
    testGetRangeHandleException Bool.toString map_even_s1 ~2 9
				
val map_100divNminus7_s1 = map (fn n => 100 div (n - 7)) s1
val len_map_100divNminus7_s1 = length map_100divNminus7_s1
val testGetRangeHandleException_map_100divNminus7_s1 = 
    testGetRangeHandleException Int.toString map_100divNminus7_s1 ~2 9

val s2 = fromList [2,3,5,7,11,17]
val len_s2 = length s2
val testGet_s2 = testGet s2		    
val toList_s2 = toList s2
val testGetRangeHandleException_s2 =
    testGetRangeHandleException Int.toString s2 ~2 8

val map_dbl_s2 = map (fn n => n*2) s2
val len_map_dbl_s2 = length map_dbl_s2
val testGet_map_dbl_s2 = testGet map_dbl_s2
val toList_map_dbl_s2 = toList map_dbl_s2
val testGetRangeHandleException_map_dbl_s2 =
    testGetRangeHandleException Int.toString map_dbl_s2 ~2 8
				 
val map_even_s2 = map (fn n => n mod 2 = 0) s2
val len_map_even_s2 = length map_even_s2
val testGet_map_even_s2 = testGet map_even_s2
val toList_map_even_s2 = toList map_even_s2
val testGetRangeHandleException_map_dbl_map_even_s2 =
    testGetRangeHandleException Bool.toString map_even_s2 ~2 8
				    
val map_100divNminus7_s2 = map (fn n => 100 div (n - 7)) s2
val len_map_100divNminus7_s2 = length map_100divNminus7_s2
val testGetRangeHandleException_map_100divNminus7_s2 = 
    testGetRangeHandleException Int.toString map_100divNminus7_s2 ~2 8

val s3 = concat (concat s1 s2)
                (concat map_dbl_s1 map_dbl_s2)
val len_s3 = length s3
val testGet_s3 = testGet s3
val toList_s3 = toList s3
val testGetRangeHandleException_s3 =
    testGetRangeHandleException Int.toString s3 ~2 28	       

val map_inc_s3 = map (fn x => x + 1) s3
val len_map_inc_s3 = length map_inc_s3
val testGet_map_inc_s3 = testGet map_inc_s3
val toList_map_inc_s3 = toList map_inc_s3
val testGetRangeHandleException_map_inc_s3 =
    testGetRangeHandleException Int.toString map_inc_s3 ~2 28

(* This function raises a suscript error for any index that's 4 mod 5 *)
fun subscriptError4Mod5 n = List.nth(Utils.range 10 14, n mod 5)
	
val map_subscriptError4Mod5_map_inc_s3 = map subscriptError4Mod5 map_inc_s3
val len_map_subscriptError4Mod5_map_inc_s3 = length map_subscriptError4Mod5_map_inc_s3
val testGetRangeHandleException_map_subscriptError4Mod5_map_inc_s3 =
    testGetRangeHandleException Int.toString map_subscriptError4Mod5_map_inc_s3 ~2 28
		       
val s4 = concat (concat s1 (concat map_dbl_s1 map_100divNminus7_s1))
                (concat (concat s2 map_dbl_s2) map_100divNminus7_s2)
val len_s4 = length s4
val testGetRangeHandleException_s4 = testGetRangeHandleException Int.toString s4 ~2 41

val map_inc_s4 = map (fn x => x + 1) s4
val len_map_inc_s4 = length map_inc_s4
val testGetRangeHandleException_map_inc_s4 =
    testGetRangeHandleException Int.toString  map_inc_s4 ~2 41

val map_subscriptError4Mod5_map_inc_s4 = map subscriptError4Mod5 map_inc_s4
val len_map_subscriptError4Mod5_map_inc_s4 = length map_subscriptError4Mod5_map_inc_s4
val testGetRangeHandleException_map_subscriptError4Mod5_map_inc_s4 =
    testGetRangeHandleException Int.toString map_subscriptError4Mod5_map_inc_s4 ~2 41

(* A slow incrementing function *)
fun linearIncrement n =
  let fun loop num ans =
	if num = 0 then ans else loop (num-1) (ans+1)
  in loop n 1
 end

(* A very slow doubling function *)
fun quadraticDouble n =
  let fun loop num ans =
	if num = 0 then ans else loop (num-1) (linearIncrement ans)
  in loop n n
 end

val reallyBigSeq = segment ~200000000 200000000 quadraticDouble
val len_reallyBigSeq = length reallyBigSeq
val testGetRange_reallyBigSeq = testGetRange reallyBigSeq 200000000 200000010

val reallyBigSeq2 = concat reallyBigSeq (map quadraticDouble reallyBigSeq)
val len_reallyBigSeq2 = length reallyBigSeq
val testGetRange1_reallyBigSeq2 = testGetRange reallyBigSeq2 200000000 200000010
val testGetRange2_reallyBigSeq2 = testGetRange reallyBigSeq2 600000000 600000010			       


					       (* Expected values for the above test cases:

val mapSeq1 = - : int t
val mapSeq2 = - : int t
val testGet_mapSeq1 = [(0,6),(1,4),(2,10),(3,5)] : (int * int) list
val testGetRange_mapSeq1 = [(1,4),(2,10)] : (int * int) list
val testGetRange_mapSeq2 = [(2,4),(3,10),(4,5)] : (int * int) list
val s1 = - : int t
val len_s1 = 7 : int
val testGet_s1 = [(0,~9),(1,~8),(2,~5),(3,0),(4,7),(5,16),(6,27)]
  : (int * int) list
val toList_s1 = [~9,~8,~5,0,7,16,27] : int list
val testGetRangeHandleException_s1 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~9"),(1,"~8"),(2,"~5"),(3,"0"),(4,"7"),(5,"16"),(6,"27"),
   (7,"Error: IndexOutOfBounds -- 7"),(8,"Error: IndexOutOfBounds -- 8")]
  : (int * string) list
val map_dbl_s1 = - : int t
val len_map_dbl_s1 = 7 : int
val testGet_map_dbl_s1 = [(0,~18),(1,~16),(2,~10),(3,0),(4,14),(5,32),(6,54)]
  : (int * int) list
val toList_map_dbl_s1 = [~18,~16,~10,0,14,32,54] : int list
val testGetRangeHandleException_map_dbl_s1 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~18"),(1,"~16"),(2,"~10"),(3,"0"),(4,"14"),(5,"32"),(6,"54"),
   (7,"Error: IndexOutOfBounds -- 7"),(8,"Error: IndexOutOfBounds -- 8")]
  : (int * string) list
val map_even_s1 = - : bool t
val len_map_even_s1 = 7 : int
val testGet_map_even_s1 =
  [(0,false),(1,true),(2,false),(3,true),(4,false),(5,true),(6,false)]
  : (int * bool) list
val toList_map_even_s1 = [false,true,false,true,false,true,false] : bool list
val testGetRangeHandleException_map_dbl_map_even_s1 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"false"),(1,"true"),(2,"false"),(3,"true"),(4,"false"),(5,"true"),
   (6,"false"),(7,"Error: IndexOutOfBounds -- 7"),
   (8,"Error: IndexOutOfBounds -- 8")] : (int * string) list
val map_100divNminus7_s1 = - : int t
val len_map_100divNminus7_s1 = 7 : int
val testGetRangeHandleException_map_100divNminus7_s1 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~7"),(1,"~7"),(2,"~9"),(3,"~15"),(4,"Error: Div -- divide by zero"),
   (5,"11"),(6,"5"),(7,"Error: IndexOutOfBounds -- 7"),
   (8,"Error: IndexOutOfBounds -- 8")] : (int * string) list
val s2 = - : int t
val len_s2 = 6 : int
val testGet_s2 = [(0,2),(1,3),(2,5),(3,7),(4,11),(5,17)] : (int * int) list
val toList_s2 = [2,3,5,7,11,17] : int list
val testGetRangeHandleException_s2 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"2"),(1,"3"),(2,"5"),(3,"7"),(4,"11"),(5,"17"),
   (6,"Error: IndexOutOfBounds -- 6"),(7,"Error: IndexOutOfBounds -- 7")]
  : (int * string) list
val map_dbl_s2 = - : int t
val len_map_dbl_s2 = 6 : int
val testGet_map_dbl_s2 = [(0,4),(1,6),(2,10),(3,14),(4,22),(5,34)]
  : (int * int) list
val toList_map_dbl_s2 = [4,6,10,14,22,34] : int list
val testGetRangeHandleException_map_dbl_s2 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"4"),(1,"6"),(2,"10"),(3,"14"),(4,"22"),(5,"34"),
   (6,"Error: IndexOutOfBounds -- 6"),(7,"Error: IndexOutOfBounds -- 7")]
  : (int * string) list
val map_even_s2 = - : bool t
val len_map_even_s2 = 6 : int
val testGet_map_even_s2 =
  [(0,true),(1,false),(2,false),(3,false),(4,false),(5,false)]
  : (int * bool) list
val toList_map_even_s2 = [true,false,false,false,false,false] : bool list
val testGetRangeHandleException_map_dbl_map_even_s2 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"true"),(1,"false"),(2,"false"),(3,"false"),(4,"false"),(5,"false"),
   (6,"Error: IndexOutOfBounds -- 6"),(7,"Error: IndexOutOfBounds -- 7")]
  : (int * string) list
val map_100divNminus7_s2 = - : int t
val len_map_100divNminus7_s2 = 6 : int
val testGetRangeHandleException_map_100divNminus7_s2 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~20"),(1,"~25"),(2,"~50"),(3,"Error: Div -- divide by zero"),(4,"25"),
   (5,"10"),(6,"Error: IndexOutOfBounds -- 6"),
   (7,"Error: IndexOutOfBounds -- 7")] : (int * string) list
val s3 = - : int t
val len_s3 = 26 : int
val testGet_s3 =
  [(0,~9),(1,~8),(2,~5),(3,0),(4,7),(5,16),(6,27),(7,2),(8,3),(9,5),(10,7),
   (11,11),(12,17),(13,~18),(14,~16),(15,~10),(16,0),(17,14),(18,32),(19,54),
   (20,4),(21,6),(22,10),(23,14),(24,22),(25,34)] : (int * int) list
val toList_s3 =
  [~9,~8,~5,0,7,16,27,2,3,5,7,11,17,~18,~16,~10,0,14,32,54,4,6,10,14,22,34]
  : int list
val testGetRangeHandleException_s3 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~9"),(1,"~8"),(2,"~5"),(3,"0"),(4,"7"),(5,"16"),(6,"27"),(7,"2"),
   (8,"3"),(9,"5"),(10,"7"),(11,"11"),(12,"17"),(13,"~18"),(14,"~16"),
   (15,"~10"),(16,"0"),(17,"14"),(18,"32"),(19,"54"),(20,"4"),(21,"6"),
   (22,"10"),(23,"14"),(24,"22"),(25,"34"),
   (26,"Error: IndexOutOfBounds -- 26"),(27,"Error: IndexOutOfBounds -- 27")]
  : (int * string) list
val map_inc_s3 = - : int t
val len_map_inc_s3 = 26 : int
val testGet_map_inc_s3 =
  [(0,~8),(1,~7),(2,~4),(3,1),(4,8),(5,17),(6,28),(7,3),(8,4),(9,6),(10,8),
   (11,12),(12,18),(13,~17),(14,~15),(15,~9),(16,1),(17,15),(18,33),(19,55),
   (20,5),(21,7),(22,11),(23,15),(24,23),(25,35)] : (int * int) list
val toList_map_inc_s3 =
  [~8,~7,~4,1,8,17,28,3,4,6,8,12,18,~17,~15,~9,1,15,33,55,5,7,11,15,23,35]
  : int list
val testGetRangeHandleException_map_inc_s3 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~8"),(1,"~7"),(2,"~4"),(3,"1"),(4,"8"),(5,"17"),(6,"28"),(7,"3"),
   (8,"4"),(9,"6"),(10,"8"),(11,"12"),(12,"18"),(13,"~17"),(14,"~15"),
   (15,"~9"),(16,"1"),(17,"15"),(18,"33"),(19,"55"),(20,"5"),(21,"7"),
   (22,"11"),(23,"15"),(24,"23"),(25,"35"),
   (26,"Error: IndexOutOfBounds -- 26"),(27,"Error: IndexOutOfBounds -- 27")]
  : (int * string) list
val subscriptError4Mod5 = fn : int -> int
val map_subscriptError4Mod5_map_inc_s3 = - : int t
val len_map_subscriptError4Mod5_map_inc_s3 = 26 : int
val testGetRangeHandleException_map_subscriptError4Mod5_map_inc_s3 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"12"),(1,"13"),(2,"11"),(3,"11"),(4,"13"),(5,"12"),(6,"13"),(7,"13"),
   (8,"Error: Subscript -- subscript out of bounds"),(9,"11"),(10,"13"),
   (11,"12"),(12,"13"),(13,"13"),(14,"10"),(15,"11"),(16,"11"),(17,"10"),
   (18,"13"),(19,"10"),(20,"10"),(21,"12"),(22,"11"),(23,"10"),(24,"13"),
   (25,"10"),(26,"Error: IndexOutOfBounds -- 26"),
   (27,"Error: IndexOutOfBounds -- 27")] : (int * string) list
val s4 = - : int t
val len_s4 = 39 : int
val testGetRangeHandleException_s4 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~9"),(1,"~8"),(2,"~5"),(3,"0"),(4,"7"),(5,"16"),(6,"27"),(7,"~18"),
   (8,"~16"),(9,"~10"),(10,"0"),(11,"14"),(12,"32"),(13,"54"),(14,"~7"),
   (15,"~7"),(16,"~9"),(17,"~15"),(18,"Error: Div -- divide by zero"),
   (19,"11"),(20,"5"),(21,"2"),(22,"3"),(23,"5"),(24,"7"),(25,"11"),(26,"17"),
   (27,"4"),(28,"6"),(29,"10"),(30,"14"),(31,"22"),(32,"34"),(33,"~20"),
   (34,"~25"),(35,"~50"),(36,"Error: Div -- divide by zero"),(37,"25"),
   (38,"10"),(39,"Error: IndexOutOfBounds -- 39"),
   (40,"Error: IndexOutOfBounds -- 40")] : (int * string) list
val map_inc_s4 = - : int t
val len_map_inc_s4 = 39 : int
val testGetRangeHandleException_map_inc_s4 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"~8"),(1,"~7"),(2,"~4"),(3,"1"),(4,"8"),(5,"17"),(6,"28"),(7,"~17"),
   (8,"~15"),(9,"~9"),(10,"1"),(11,"15"),(12,"33"),(13,"55"),(14,"~6"),
   (15,"~6"),(16,"~8"),(17,"~14"),(18,"Error: Div -- divide by zero"),
   (19,"12"),(20,"6"),(21,"3"),(22,"4"),(23,"6"),(24,"8"),(25,"12"),(26,"18"),
   (27,"5"),(28,"7"),(29,"11"),(30,"15"),(31,"23"),(32,"35"),(33,"~19"),
   (34,"~24"),(35,"~49"),(36,"Error: Div -- divide by zero"),(37,"26"),
   (38,"11"),(39,"Error: IndexOutOfBounds -- 39"),
   (40,"Error: IndexOutOfBounds -- 40")] : (int * string) list
val map_subscriptError4Mod5_map_inc_s4 = - : int t
val len_map_subscriptError4Mod5_map_inc_s4 = 39 : int
val testGetRangeHandleException_map_subscriptError4Mod5_map_inc_s4 =
  [(~2,"Error: IndexOutOfBounds -- ~2"),(~1,"Error: IndexOutOfBounds -- ~1"),
   (0,"12"),(1,"13"),(2,"11"),(3,"11"),(4,"13"),(5,"12"),(6,"13"),(7,"13"),
   (8,"10"),(9,"11"),(10,"11"),(11,"10"),(12,"13"),(13,"10"),
   (14,"Error: Subscript -- subscript out of bounds"),
   (15,"Error: Subscript -- subscript out of bounds"),(16,"12"),(17,"11"),
   (18,"Error: Div -- divide by zero"),(19,"12"),(20,"11"),(21,"13"),
   (22,"Error: Subscript -- subscript out of bounds"),(23,"11"),(24,"13"),
   (25,"12"),(26,"13"),(27,"10"),(28,"12"),(29,"11"),(30,"10"),(31,"13"),
   (32,"10"),(33,"11"),(34,"11"),(35,"11"),
   (36,"Error: Div -- divide by zero"),(37,"11"),(38,"11"),
   (39,"Error: IndexOutOfBounds -- 39"),(40,"Error: IndexOutOfBounds -- 40")]
  : (int * string) list
val linearIncrement = fn : int -> int
val quadraticDouble = fn : int -> int
val reallyBigSeq = - : int t
val len_reallyBigSeq = 400000000 : int
val testGetRange_reallyBigSeq =
  [(200000000,0),(200000001,2),(200000002,4),(200000003,6),(200000004,8),
   (200000005,10),(200000006,12),(200000007,14),(200000008,16),(200000009,18)]
  : (int * int) list
val reallyBigSeq2 = - : int t
val len_reallyBigSeq2 = 400000000 : int
val testGetRange1_reallyBigSeq2 =
  [(200000000,0),(200000001,2),(200000002,4),(200000003,6),(200000004,8),
   (200000005,10),(200000006,12),(200000007,14),(200000008,16),(200000009,18)]
  : (int * int) list
val testGetRange2_reallyBigSeq2 =
  [(600000000,0),(600000001,4),(600000002,8),(600000003,12),(600000004,16),
   (600000005,20),(600000006,24),(600000007,28),(600000008,32),(600000009,36)]
  : (int * int) list
val it = () : unit
- 

*)					       
