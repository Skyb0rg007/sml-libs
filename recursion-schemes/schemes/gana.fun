
functor GAnamorphism(
  structure F: FUNCTOR
  structure M: MONAD
  type t
  val embed: t F.t -> t
  val dist: 'a F.t M.t -> 'a M.t F.t):
sig
  val ana: ('a -> 'a M.t F.t) -> 'a -> t
end =
struct
  fun ana coalg =
    let
      fun go x = embed (F.map (go o M.map coalg o M.join) (dist x))
    in
      go o M.pure o coalg
    end
end
