
functor EitherMonadFn(
  structure M: MONAD
  type t) :> EITHER_MONAD where type e = t and type 'a M.t = 'a M.t =
struct
  structure M = M
  type e = t
  type 'a t = (e, 'a) Either.either M.t

  fun throw e = M.pure (Either.INL e)

  fun lift m = M.map Either.INR m

  fun map f = M.map (Either.map (Fn.id, f))

  fun pure x = M.pure (Either.INR x)

  fun join m =
    M.join
    (M.map
      (fn Either.INL e => M.pure (Either.INL e)
        | Either.INR x => x)
      m)
end
