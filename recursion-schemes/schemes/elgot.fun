
functor Elgot(structure F: FUNCTOR):
sig
  val elgot: ('b F.t -> 'b) -> ('a -> ('b, 'a F.t) Either.either) -> 'a -> 'b
end =
struct
  fun elgot alg coalg =
    let
      fun go x =
        case coalg x of
            Either.INL x => x
          | Either.INR x => alg (F.map go x)
    in
      go
    end
end
