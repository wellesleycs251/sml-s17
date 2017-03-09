signature STRING_SET =
sig
    (* The type of sets *)
    type t

    (* An empty set *)
    val empty : t 

    (* Construct a single-element set from that element. *)
    val singleton : string -> t

    (* Check if a set is empty. *)
    val isEmpty : t -> bool

    (* Return the number of elements in the set. *)
    val size : t -> int

    (* Check if a given element is a member of the given set. *)
    val member : string -> t -> bool

    (* Construct a set containing the given element and all elements
       of the given set. *)
    val insert : string -> t -> t

    (* Construct a set containing all elements of the given set
       except for the given element. *)
    val delete : string -> t -> t

    (* Construct the union of two sets. *)
    val union : t -> t -> t

    (* Construct the intersection of two sets. *)
    val intersection : t -> t -> t

    (* Construct the symmetric difference of two sets. *)
    val difference : t -> t -> t

    (* Construct a set from a list of elements.
       Do not assume the list elements are unique. *)
    val fromList : string list -> t 

    (* Convert a set to a list without duplicates. 
       The elements in the resulting list may be in any order. *)
    val toList : t -> string list

    (* Convert a set to a predicate function. *)
    val toPred : t -> (string -> bool)

    (* Convert a set to a string representation, given a function
       that converts a set element into a string representation. *)
    val toString : t -> string
			      
end


(* Implement a SET ADT using lists to represent sets. *)
structure StringSetList :> STRING_SET = struct

    (* Sets are represented by lists of ordered strings without duplicates. *)
    type t = string list

    (* The empty set is the empty list. *)
    val empty = []

    fun singleton x = [x]
			    
    fun isEmpty [] = true
      | isEmpty _  = false
      
    fun size xs = length xs
		       
    fun member x ys = List.exists (fn y => y=x) ys
			 
    fun insert x [] = [x]
      | insert x (ys as y::ys') =
	(case (String.compare(x,y)) of
		 LESS => x :: ys
	       | EQUAL => ys
	       | GREATER => y :: (insert x ys'))
				     
    fun delete x [] = []
      | delete x (ys as y::ys') =
	(case (String.compare(x,y)) of
		 LESS => ys
	       | EQUAL => ys'
	       | GREATER => y :: (delete x ys'))

    fun union xs [] = xs (* a merge of two sorted lists *)
      | union [] ys = ys
      | union (xs as x::xs') (ys as y::ys') =
	(case (String.compare(x,y)) of
	     LESS => x :: union xs' ys
	   | EQUAL => x :: union xs' ys'
	   | GREATER => y :: union xs ys')
      
    fun intersection xs [] = []
      | intersection [] ys = []
      | intersection (xs as x::xs') (ys as y::ys') =
	(case (String.compare(x,y)) of
	     LESS => intersection xs' ys
	   | EQUAL => x :: intersection xs' ys'
	   | GREATER => intersection xs ys')
			    
    fun difference xs [] = xs
      | difference [] ys = []
      | difference (xs as x::xs') (ys as y::ys') =
	(case (String.compare(x,y)) of
	     LESS => x :: difference xs' ys
	   | EQUAL => difference xs' ys'
	   | GREATER => difference xs ys')

    (* This is insertion sort. 
       Should really use n log n sort here! *)			
    fun fromList xs = foldl (fn (x,set) => insert x set) empty xs
			   
    fun toList xs = xs
			 
    fun toPred xs = fn y => List.exists (fn x => x = y) xs

    fun toString xs = "{" ^ (String.concatWith "," xs) ^ "}"

end

(*
open StringSetList

(* Testing *)

fun split string = String.tokens (fn s => s = #" ") string

val s1 = fromList (split "c a b d a b c e d a b")
val s2 = fromList (split "c a f g c f e g a c")
val s1UnionS2 = union s1 s2
val s1IntersectionS2 = intersection s1 s2
val s1DifferenceS2 = difference s1 s2
val s2DifferenceS1 = difference s2 s1
val intersectionDiffs = intersection s1DifferenceS2 s2DifferenceS1
val sets = [s1, s2, s1UnionS2, s1IntersectionS2, s1DifferenceS2, s2DifferenceS1, intersectionDiffs]
val toLists = map toList sets
val toStrings = map toString sets		  
*)	       
					    
