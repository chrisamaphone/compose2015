signature EQ = sig
  type t 
  val eq : t -> t -> bool
end
structure EqInt : EQ =
struct
  type t = int
  fun eq x y = x = y
end
functor EqProd (X : EQ) (Y : EQ) : EQ = struct
  type t = X.t * Y.t
  fun eq (x1,y1) (x2,y2) =
    (X.eq x1 x2) andalso (Y.eq y1 y2)
end
structure EqIntPair = EqProd (EqInt) (EqInt)
