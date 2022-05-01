
signature FREE_COMONAD =
sig
  structure F: FUNCTOR

  datatype 'a t = :< of 'a * 'a t F.t

  val unwrap: 'a t -> 'a t F.t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val extract: 'a t -> 'a
  val extend: ('a t -> 'b) -> 'a t -> 'b t
  val duplicate: 'a t -> 'a t t
end
