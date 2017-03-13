(* Binary search trees on integers *)
(* Uses the bintree datatype *)

use "bintree-soln.sml";

fun singleton v = ()

fun insert x Leaf = ()
  | insert x (t as (Node(l,v,r))) = ()

fun listToTree xs = (* Hint: use foldl *)
  ()

fun member x Leaf = ()
  | member x (Node(l,v,r)) = ()

    
(* Test cases *)								     

(* val test_bst = listToTree [4,2,3,6,1,7,5]; *) 

(* val test_member = map (fn i => (i, member i test_bst)) [0,1,2,3,4,5,6,7,8] *)


