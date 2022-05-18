(* The Identity functor/monad/comonad *)

structure Identity:
sig
  type 'a t = 'a

  val map: ('a -> 'b) -> 'a t -> 'b t
  val pure: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val ap: ('a -> 'b) t * 'a t -> 'b t
  val join: 'a t t -> 'a
  val extract: 'a t -> 'a
  val duplicate: 'a t -> 'a t t
  val extend: ('a t -> 'b) -> 'a t -> 'b t
end =
struct
  type 'a t = 'a

  fun map f x = f x

  fun pure x = x
  fun bind x f = f x
  fun ap (f, x) = f x
  fun join x = x

  fun extract x = x
  fun duplicate x = x
  fun extend f x = f x
end
