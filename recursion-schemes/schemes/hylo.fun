
functor Hylomorphism(
  structure F: FUNCTOR):
sig
  val hylo: ('b F.t -> 'b) -> ('a -> 'a F.t) -> 'a -> 'b
end =
struct
  structure H = GHylomorphism(
    struct
      structure F = F
      structure M = Identity
      structure W = Identity
      fun distW x = x
      fun distM x = x
    end)

  val hylo = H.hylo
end
