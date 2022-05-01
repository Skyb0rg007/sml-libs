
signature COFREE =
sig
  structure F: FUNCTOR

  datatype 'a t = :< of 'a * 'a t F.t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val extract: 'a t -> 'a
  val extend: ('a t -> 'b) -> 'a t -> 'b t
  val duplicate: 'a t -> 'a t t
end

functor CofreeFn(F: FUNCTOR) :> COFREE where type 'a F.t = 'a F.t =
struct
  infix 5 :<
  structure F = F

  datatype 'a t = :< of 'a * 'a t F.t

  fun map f (a :< fa) = f a :< F.map (map f) fa

  fun extract (a :< _) = a

  fun extend f (w as _ :< fa) = f w :< F.map (extend f) fa

  fun duplicate (w as _ :< fa) = w :< F.map duplicate fa
end
