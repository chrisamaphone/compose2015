signature STACK =
sig
  exception E
  type 'a stack
  val new : 'a stack
  val push : 'a -> 'a stack -> 'a stack
  val pop : 'a stack -> 'a stack
  val top : 'a stack -> 'a
  (* step 2 *)
  val size : 'a stack -> int
end

structure ListStack : STACK =
struct
  exception E
  type 'a stack = 'a list
  val new = []
  fun push x s = x::s
  fun split (h::t) = (h,t)
    | split _ = raise E
  fun pop s = #2(split s)
  fun top s = #1(split s)
  (* *** *)
  fun size l = List.length l
end

(* Run these in the repl:
val emptyStack = ListStack.new;
val stack0 = ListStack.push 0 emptyStack;
*)

(* maintain constant size *)
structure SizedStack : STACK =
struct
  exception E
  type 'a stack = ('a list * int)
  val new = ([], 0)
  fun push x (stack, size) = (x::stack, size + 1)
  fun split (h::t) = (h,t)
    | split _ = raise E
  fun pop (stack, size) = (#2(split stack), size - 1)
  fun top (stack, size) = #1(split stack)
  fun size (stack, sz) = sz
end

