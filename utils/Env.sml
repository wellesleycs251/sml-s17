signature ENV = sig
  type 'a env
  val empty: 'a env
  val bind : string -> 'a -> 'a env -> 'a env
  val bindAll : string list -> 'a list -> 'a env -> 'a env
  val make : string list -> 'a list -> 'a env 
  val lookup : string -> 'a env -> 'a option
  val map: ('a -> 'a) -> 'a env -> 'a env
  val remove : string -> 'a env -> 'a env
  val removeAll : string list -> 'a env -> 'a env
  val merge : 'a env -> 'a env -> 'a env
end

(* Association list representation of environments 
   An environment is represented as a list of (name, value) pairs.
   A pair earlier in the list shadows a value later in the list. *)
structure Env :> ENV = struct

type 'a env = (string * 'a) list

val empty = []

fun bind name valu env = (name, valu) :: env

fun bindAll names vals env = foldr (fn ((n,v),e) => bind n v e)
				   env
				   (ListPair.zip(names,vals))

fun make names vals = bindAll names vals empty
			      
fun map f env = List.map (fn (name, value) => (name, f value)) env

fun lookup name env =
  (case List.find (fn (n,v) => n = name) env of
       SOME (n,v) => SOME v
     | NONE => NONE)

fun remove name env = List.filter (fn (n,_) => n <> name) env
				  
fun removeAll names env = foldr (fn (n,e) => remove n e) env names
				
fun merge env1 env2 = env1 @ env2

end
