
signature COMONAD =
sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val extract: 'a t -> 'a
  val extend: ('a t -> 'b) -> 'a t -> 'b t
  val duplicate: 'a t -> 'a t t
end
