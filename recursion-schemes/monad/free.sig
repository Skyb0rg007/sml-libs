
signature FREE_MONAD =
sig
  structure F: FUNCTOR

  datatype 'a t =
     Pure of 'a 
   | Impure of 'a t F.t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val pure: 'a -> 'a t
  val ap: ('a -> 'b) t * 'a t -> 'b t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val join: 'a t t -> 'a t
end
