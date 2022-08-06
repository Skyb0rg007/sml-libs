
signature PAIR_COMONAD =
sig
  type left
  type 'a t = left * 'a

  val map: ('a -> 'b) -> 'a t -> 'b t
  val extract: 'a t -> 'a
  val extend: ('a t -> 'b) -> 'a t -> 'b t
  val duplicate: 'a t -> 'a t t
end

functor PairComonad(type t) :> PAIR_COMONAD =
struct
  type 'a t = t * 'a

  fun map f (x, y) = (x, f y)
  fun extract (_, y) = y
  fun extend f (x, y) = (x, f (x, y))
  fun duplicate (x, y) = (x, (x, y))
end
