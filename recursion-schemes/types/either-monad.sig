
signature EITHER_MONAD =
sig
  structure M: MONAD
  type e
  type 'a t = (e, 'a) Either.either M.t

  (* MonadExcept *)
  val throw: e -> 'a t
  (* MonadTrans *)
  val lift: 'a M.t -> 'a t
  (* Monad *)
  val map: ('a -> 'b) -> 'a t -> 'b t
  val pure: 'a -> 'a t
  val join: 'a t t -> 'a t
end
