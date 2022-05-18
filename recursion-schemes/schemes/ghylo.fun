
functor GHylomorphism(
  structure F: FUNCTOR
  structure M: MONAD
  structure W: COMONAD
  val distW: 'a W.t F.t -> 'a F.t W.t
  val distM: 'a F.t M.t -> 'a M.t F.t):
sig
  val hylo: ('b W.t F.t -> 'b) -> ('a -> 'a M.t F.t) -> 'a -> 'b
end =
struct
  fun hylo alg coalg =
    let
      val alg' = W.map alg o distW o F.map W.duplicate
      val coalg' = F.map M.join o distM o M.map coalg
      fun go x = alg' (F.map go (coalg' x))
    in
      alg o F.map go o coalg
    end
end
