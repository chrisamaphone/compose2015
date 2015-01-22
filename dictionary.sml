signature ORDERED =
sig
  type t
  val compare : t * t -> order (* LESS, EQUAL, or GREATER *)
end

signature DICTIONARY =
sig
  type key
  type 'v dict

  val empty : 'v dict

  (* insert D (k,v) 
  *   inserts the key-value pair (k,v) into D
  *   and returns the new dictionary. 
  *  
  *  If key k already exists in d, replace the existing mapping with (k,v).
  *)
  val insert : 'v dict -> key * 'v -> 'v dict

  (* lookup D k = SOME v   if k maps to v in D
  *             = NONE     if k is not in D. *)
  val lookup : 'v dict -> key -> 'v option

  (* remove D k = SOME D' if removing k from D = D'
  *             = NONE    if k is not in D. *)
  val remove : 'v dict -> key -> 'v dict option
end

functor ListDict (Key : ORDERED) : DICTIONARY =
struct
  type key = Key.t
  type 'v dict = (Key.t * 'v) list

  val empty = nil

  fun insert D (k,v) = (k,v)::D

  fun lookup nil k = NONE
    | lookup ((k',v)::D) k = 
      (case Key.compare (k,k') of EQUAL => SOME v | _ => lookup D k)

  fun remove nil k = NONE
    | remove ((k',v)::D) k =
      (case Key.compare (k,k') of
            EQUAL => (case remove D k of NONE => SOME D | SOME D' => SOME D')
          | _ => (case remove D k of NONE => NONE | SOME D' => SOME D'))
end

structure IntOrdered : ORDERED =
struct
  type t = int
  val compare = Int.compare
end

structure IntListDict = ListDict(IntOrdered)
