(* Hofl Environments (with a fixpoint operator) *)

signature ENV = sig
  type 'a env
  val empty: 'a env
  val bind : string -> 'a -> 'a env -> 'a env
  val bindAll : string list -> 'a list -> 'a env -> 'a env
  val make : string list -> 'a list -> 'a env 
  val lookup : string -> 'a env -> 'a option
  val bindThunk : string -> (unit -> 'a) -> 'a env -> 'a env
  val bindAllThunks : string list -> (unit -> 'a) list -> 'a env -> 'a env
  val merge : 'a env -> 'a env -> 'a env
  val fix : ('a env -> 'a env) -> 'a env 
  (* for testing *)
  val fromFun : (string -> 'a option) -> 'a env
  val toFun : 'a env -> (string -> 'a option)
end 

structure Env :> ENV = struct
  type 'a env = string -> 'a option 
  val empty = fn s => NONE
  fun bind name value env = fn s => if s = name then SOME value else env s
  fun bindAll names vals env =
    List.foldr (fn((name,value),env)  => bind name value env)
	       env (ListPair.zip(names,vals))
  fun make names vals = bindAll names vals empty
  fun lookup name env = env name  
  (* New for HOFL environments *)
  fun bindThunk name valuThunk env = 
    fn s => if s = name then SOME (valuThunk ()) else env s
  fun bindAllThunks names valThunks env =
    List.foldr (fn((name,valThunk),env) => bindThunk name valThunk env)
	       env (ListPair.zip(names,valThunks))
  fun merge env1 env2 =
    fn s => (case env1 s of
		 NONE => env2 s
               | some => some)
  fun fix gen = let fun envfix s = (gen envfix) s
		in envfix
		end
  (* for testing *)
  fun fromFun f = f
  fun toFun f = f
end
