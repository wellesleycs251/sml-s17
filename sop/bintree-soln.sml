datatype 'a bintree =
    Leaf
  | Node of 'a bintree * 'a * 'a bintree (* left subtree, value, right subtree *)

val int_tree = Node(Node(Leaf,2,Leaf),
		    4, 
		    Node(Node(Leaf, 1, Node(Leaf, 5, Leaf)),
			 6,
			 Node(Leaf, 3, Leaf)))

val string_tree = Node(Node (Leaf,"like",Leaf),
		       "green",     
		       Node (Node (Leaf,"eggs",Leaf),
			     "and",
			     Node (Leaf,"ham",Leaf)))

(* val num_nodes = fn : 'a bintree -> int *)
(* Returns the number of nodes in a binary tree *)
fun num_nodes Leaf = 0
  | num_nodes (Node(l,v,r)) = 1 + (num_nodes l) + (num_nodes r)

(* val height = fn : 'a bintree -> int *)
(* Returns the height of a binary tree *)
fun height Leaf = 0
  | height (Node(l,v,r)) = 1 + Int.max(height l, height r)

(* val sum_nodes = fn : int bintree -> int *)
(* Returns the sum of node values in binary tree of ints *)
fun sum_nodes Leaf = 0
  | sum_nodes (Node(l,v,r)) = (sum_nodes l) + v + (sum_nodes r)

(* val inlist = fn : 'a bintree -> 'a list *)
(* Returns a list of the node values in in-order *)
fun inlist Leaf = []
  | inlist (Node(l,v,r)) = (inlist l) @ [v] @ (inlist r)

(* val map_tree = fn : ('a -> 'b) -> 'a bintree -> 'b bintree *)
(* maps function over every node in a binary tree *)
fun map_tree f Leaf = Leaf
  | map_tree f (Node(l,v,r)) = Node(map_tree f l, f v, map_tree f r)

(* val fold_tree = fn : ('b * 'a * 'b -> 'b) -> 'b -> 'a bintree -> 'b *)
(* binary tree accumulation *)
fun fold_tree comb leafval Leaf = leafval
  | fold_tree comb leafval (Node(l,v,r)) =
    comb(fold_tree comb leafval l, v, fold_tree comb leafval r)


