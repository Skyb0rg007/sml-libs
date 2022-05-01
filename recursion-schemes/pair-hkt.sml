
signature HKTMonad =
sig
  type t

  val map: ('a -> 'b) -> ('a, t) HKT.app -> ('b, t) HKT.app
end

structure PairHKT =
struct
  structure H = Newtype2(type ('a, 'b) t = 'a * 'b)

end
