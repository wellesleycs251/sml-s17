(* Put your name here:    *)

(* CS 251 Fall '17 Problem Set 7 Problem 5 OperationTreeSet. 

   Complete the operation-tree representation of the SET signature.
   Represent a set as a tree of set operations. 

   You may ignore "Warning: calling polyEqual" in this exercise. *)

exception Unimplemented (* Placeholder during development. *)
exception Unimplementable (* Impossible to implement *)

(* SET describes operations over set values of type ''a t, where set
   elements are of type ''a.  The double-quote type variable ''a means 
   that values of the type ''a can be compared using the = operation.

   Naming the type of the ADT t is a common idiom for signatures
   defining an ADT.  This means that for particular implementations
   (e.g., ListSet or FunSet), ADT values have type ListSet.t or
   FunSet.t, rather than the more verbose ListSet.set or FunSet.set.
   If a signature defines multiple types (especially if there's not
   one main type and other supporting types), this idiom is less
   commonly used. *)

signature SET =
sig
    (* The type of sets *)
    type ''a t 

    (* An empty set *)
    val empty : ''a t

    (* Construct a single-element set from that element. *)
    val singleton : ''a -> ''a t 

    (* Construct a set from a list of elements.
       Do not assume the list elements are unique. *)
    val fromList : ''a list -> ''a t 

    (* Convert a set to a list without duplicates. 
       The elements in the resulting list may be in any order. *)
    val toList : ''a t -> ''a list

    (* Check if a set is empty. *)
    val isEmpty : ''a t -> bool

    (* Return the number of elements in the set. *)
    val size : ''a t -> int

    (* Check if a given element is a member of the given set. *)
    val member : ''a -> ''a t -> bool

    (* Construct a set containing the given element and all elements
       of the given set. *)
    val insert : ''a -> ''a t -> ''a t

    (* Construct a set containing all elements of the given set
       except for the given element. *)
    val delete : ''a -> ''a t -> ''a t

    (* Construct the union of two sets. *)
    val union : ''a t -> ''a t -> ''a t

    (* Construct the intersection of two sets. *)
    val intersection : ''a t -> ''a t -> ''a t
					     
    (* Construct the difference of two sets
       (all elements in the first set but not in the second.) *)				    
    val difference : ''a t -> ''a t -> ''a t

    (* Construct a set from a predicate function:
       the resulting set should contain all elements for which
       this predicate function returns true.

       This acts like math notation for sets.  For example:
         { x | x mod 3 = 0 }
       would be written:
         fromPred (fn x => x mod 3 = 0)
    *)
    val fromPred : (''a -> bool) -> ''a t

    (* Convert a set to a predicate function. *)
    val toPred : ''a t -> ''a -> bool

    (* Convert a set to a string representation, given a function
       that converts a set element into a string representation. *)
    val toString : (''a -> string) -> ''a t -> string

end

(* Implement a SET ADT using membership predicates to represent sets. *)

structure OperationTreeSet :> SET = struct

    datatype ''a t = Empty
		   | Insert of ''a * ''a t
		   | Delete of ''a * ''a t
		   | Union of ''a t * ''a t
		   | Intersection of ''a t * ''a t
		   | Difference of ''a t * ''a t
					       
    val empty = Empty
    fun insert x s = Insert(x,s)
    fun singleton x = Insert(x, Empty)
    fun delete x s = Delete(x, s)
    fun union s1 s2 = Union(s1,s2)
    fun intersection s1 s2 = Intersection(s1,s2)
    fun difference s1 s2 = Difference(s1,s2)

    fun member _ _ = raise Unimplemented  
    fun toList _ = raise Unimplemented
    fun isEmpty _ = raise Unimplemented
    fun size _  = raise Unimplemented
    fun toPred _ = raise Unimplemented
    fun toString eltToString _ = raise Unimplemented
    fun fromList _ = raise Unimplemented

    fun fromPred _ = raise Unimplementable

end

open OperationTreeSet

(* Some test cases; feel free to add more: *)
	 
(* range helper function *)			      
fun range lo hi = if lo >= hi then [] else lo::(range (lo + 1) hi)

(* (* Tests involving "small" sets defined without fromList *)						   

(* Some small sets defined without fromList *)
val s = (delete 4 (difference (union (union (insert 1 empty)
                                            (insert 4 empty))
                              (union (insert 7 empty)
                                     (insert 4 empty)))
                  (intersection (insert 1 empty)
                                (union (insert 1 empty)
                                       (insert 6 empty)))))
val s1 = insert 17 (insert 19 (insert 23 (singleton 42)))
val s2 = insert 17 (insert 23 (insert 42 (insert 57 (insert 97 (empty)))))
val s3 = delete 19 (delete 57 (delete 85 s2))
val dupSet = insert 3 (insert 2 (insert 3 (insert 1 (insert 3 (insert 2 (empty))))))
		
val s1UnionS2 = union s1 s2
val s1IntersectionS2 = intersection s1 s2
val s1DifferenceS2 = difference s1 s2
val s2DifferenceS1 = difference s2 s1
val intersectionSmallDiffs = intersection s2DifferenceS1 s1DifferenceS2

val smallSets = [
    ("s", s), 
    ("s1", s1), 
    ("s2", s2),
    ("s3", s3),
    ("dupSet", dupSet),
    ("s1UnionS2", s1UnionS2), 
    ("s1IntersectionS2", s1IntersectionS2),
    ("s1DifferenceS2", s1DifferenceS2),
    ("s2DifferenceS1", s2DifferenceS1), 
    ("intersectionSmallDiffs", intersectionSmallDiffs)
]

(* Be sure to print all details *)			
val _ = Control.Print.printLength := 101;
val _ = Control.Print.stringDepth := 1000;		    

(* Tests of other operations on small sets *)
fun mapNamedSet f namedSets = map (fn (name,set) => (name,f set)) namedSets
fun testMember set = List.filter (fn i => member i set) (range 0 101)
fun testPred set = List.filter (toPred set) (range 0 101)
			       
val smallMembers = mapNamedSet testMember smallSets		    		    
val smallLists = mapNamedSet toList smallSets		    		    
val smallIsEmpties = mapNamedSet isEmpty smallSets		    
val smallSizes = mapNamedSet size smallSets
val smallPreds = mapNamedSet testPred smallSets
val smallStrings = mapNamedSet (fn s => toString Int.toString s) smallSets

*)


(* (* Tests involving "big" sets defined with fromList *)						   						   
val lowSet = fromList (range 0 61)
val highSet = fromList (range 40 101)
val lowUnionHighSet = union lowSet highSet
val lowInsersectionHighSet = intersection lowSet highSet
val lowDifferenceHighSet = difference lowSet highSet
val highDifferenceLowSet = difference highSet lowSet
val intersectionLowHighDiffsSet = intersection lowDifferenceHighSet highDifferenceLowSet
val mod2Set = fromList (List.filter (fn x => x mod 2 = 0) (range 0 101))
val mod3Set = fromList (List.filter (fn x => x mod 3 = 0) (range 0 101))
val mod2UnionMod3Set = union mod2Set mod3Set
val mod2IntersectionMod3Set = intersection mod2Set mod3Set
val mod2DifferenceMod3Set = difference mod2Set mod3Set
val mod3DifferenceMod2Set = difference mod3Set mod2Set
val intersectionModDiffsSet = intersection mod2DifferenceMod3Set mod3DifferenceMod2Set
val bigIntersectionSet = intersection (intersection lowSet highSet)
				      (intersection mod2Set mod3Set)
val bigDifferenceSet = difference (difference lowSet highSet)
  				  (difference mod2Set mod3Set)

val bigSets = [
    ("lowSet", lowSet),
    ("highSet", highSet),
    ("lowUnionHighSet", lowUnionHighSet),
    ("lowInsersectionHighSet", lowInsersectionHighSet),
    ("lowDifferenceHighSet", lowDifferenceHighSet),
    ("highDifferenceLowSet", highDifferenceLowSet),
    ("intersectionLowHighDiffsSet", intersectionLowHighDiffsSet),
    ("mod2Set", mod2Set),
    ("mod3Set", mod3Set),
    ("mod2UnionMod3Set", mod2UnionMod3Set),
    ("mod2IntersectionMod3Set", mod2IntersectionMod3Set),
    ("mod2DifferenceMod3Set", mod2DifferenceMod3Set),
    ("intersectionModDiffsSet", intersectionModDiffsSet),
    ("mod3DifferenceMod2Set", mod3DifferenceMod2Set),
    ("bigIntersectionSet", bigIntersectionSet),
    ("bigDifferenceSet", bigDifferenceSet)
]

val bigMembers = mapNamedSet testMember bigSets		    		    
val bigLists = mapNamedSet toList bigSets		    		    
val bigIsEmpties = mapNamedSet isEmpty bigSets		    
val bigSizes = mapNamedSet size bigSets
val bigPreds = mapNamedSet testPred bigSets
val bigStrings = mapNamedSet (fn s => toString Int.toString s) bigSets

*)		       
