signature SCHEDULER =
sig
  type 'a t
  val emp : 'a t
  val put : 'a t -> 'a -> 'a t
  val take : 'a t -> ('a * 'a t) option
end

structure Stack : SCHEDULER =
struct
  type 'a t = 'a list
  val emp = []
  fun put s x = x::s
  fun take (x::s) = SOME (x,s)
    | take [] = NONE
end

structure Queue : SCHEDULER =
struct
  type 'a t = 'a list
  val emp = []
  fun put q x = q @ [x]
  fun take (x::q) = SOME (x,q)
    | take [] = NONE
end

datatype 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

functor Traverse (S : SCHEDULER) =
struct
  fun traverse (tr : 'a tree) : 'a list =
  let
    fun traverse' (worklist : ('a tree) S.t) =
      (case S.take worklist of
           NONE => []
         | SOME (Leaf, rest) => traverse' rest
         | SOME (Node(t1,v,t2), rest) =>
              v :: (traverse' (S.put (S.put rest t1) t2))
      )
  in
    traverse' (S.put S.emp tr)
  end

end

structure DFS = Traverse (Stack)
structure BFS = Traverse (Queue)
