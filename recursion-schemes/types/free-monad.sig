
signature FREE_MONAD =
sig
  structure F: FUNCTOR

  datatype 'a t =
     Pure of 'a 
   | Impure of 'a t F.t

  val wrap: 'a t F.t -> 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val pure: 'a -> 'a t
  val join: 'a t t -> 'a t

  val dist: 'a F.t t -> 'a t F.t
end
