
functor Apomorphism(
  structure F: FUNCTOR
  type t
  val project: t -> t F.t
  val embed: t F.t -> t):
sig
  val apo: ('a -> (t, 'a) Either.either F.t) -> 'a -> t
end =
struct
  fun dist (Either.INL x) = F.map Either.INL (project x)
    | dist (Either.INR x) = F.map Either.INR x

  structure A = GAnamorphism(
    struct
      structure F = F
      structure M = EitherMonadFn(
        struct
          structure M = Identity
          type t = t
        end)
      type t = t
      val embed = embed
      val dist = dist
    end)

  val apo = A.ana
end
