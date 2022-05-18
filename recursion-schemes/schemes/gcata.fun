
functor GCatamorphism(
  structure F: FUNCTOR
  structure W: COMONAD
  type t
  val project: t -> t F.t
  val dist: 'a W.t F.t -> 'a F.t W.t):
sig
  val cata: ('a W.t F.t -> 'a) -> t -> 'a
end =
struct
  fun cata alg =
    let
      fun go x = dist (F.map (W.duplicate o W.map alg o go) (project x))
    in
      alg o W.extract o go
    end
end
