datatype 'a mylist = Nil | Cons of 'a * 'a mylist
			   
val ints = Cons(1, Cons(2, Cons(3, Nil)))
	       
val chars = Cons (#"a", Cons (#"b", Cons (#"c", Nil)))

val strings = Cons("foo", Cons ("bar", Cons ("baz", Nil)))

fun myMap f Nil = Nil
  | myMap f (Cons(x,xs)) = Cons(f x, myMap f xs)

val incNums =  myMap (fn x => x + 1) ints

val myStrings = myMap (fn s => "my " ^ s) strings
