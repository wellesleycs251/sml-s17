(* Binary search trees on integers *)
(* Uses the bintree datatype *)

fun singleton v = Node(Leaf, v, Leaf)

fun insert x Leaf = singleton x
  | insert x (t as (Node(l,v,r))) =
    if x = v then t
    else if x < v then Node(insert x l, v, r)
    else Node(l, v, insert x r)

fun listToTree xs = foldl (fn (x,t) => insert x t) Leaf xs

fun member x Leaf = false
  | member x (Node(l,v,r)) = (x = v) orelse member x l orelse member x r

(* Test cases *)								     

val test_bst = listToTree [4,2,3,6,1,7,5];

val test_member = map (fn i => (i, member i test_bst)) [0,1,2,3,4,5,6,7,8]
