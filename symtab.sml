signature SYMBOL =
sig
  type t
  val eq : t * t -> bool
  val insert : string -> t
  val lookup : t -> string
end

structure StringSym : SYMBOL =
struct
  type t = string
  
  fun eq (s1,s2) = s1 = s2

  fun insert s = s
  fun lookup s = s
end

functor Symbol (Prefix : sig val prefix : string end) :> SYMBOL =
struct
  type t = int
  fun eq (x,y) = x = y
  
  val size = ref 0
  
  val table : string list ref = ref nil

  fun insert str =
  let
  in
    size := !size + 1;
    table := (Prefix.prefix^str)::(!table);
    !size
  end

  fun lookup n = List.nth (!table, !size - n)
end

structure Temps = Symbol (struct val prefix = "%t_" end)
structure Labels = Symbol (struct val prefix = "l_" end)



