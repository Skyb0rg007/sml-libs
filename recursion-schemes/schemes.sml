
signature RECURSIVE =
sig
  structure Base: FUNCTOR

  type t

  val project: t -> t Base.t
end

signature CORECURSIVE =
sig
  structure Base: FUNCTOR

  type t

  val embed: t Base.t -> t
end

(* Generalized fold *)
functor GCatamorphism(
  structure T: RECURSIVE
  structure W: COMONAD
  val dist: 'a W.t T.Base.t -> 'a T.Base.t W.t):
sig
  val cata: ('a W.t T.Base.t -> 'a) -> T.t -> 'a
end =
struct
  fun cata alg =
    let
      fun go x = dist (T.Base.map (W.duplicate o W.map alg o go) (T.project x))
    in
      alg o W.extract o go
    end
end

(* Generalized unfold *)
functor GAnamorphism(
  structure T: CORECURSIVE
  structure M: MONAD
  val dist: 'a T.Base.t M.t -> 'a M.t T.Base.t):
sig
  val ana: ('a -> 'a M.t T.Base.t) -> 'a -> T.t
end =
struct
  fun ana coalg =
    let
      fun go x = T.embed (T.Base.map (go o M.map coalg o M.join) (dist x))
    in
      go o M.pure o coalg
    end
end

(* Generalized fold+unfold *)
functor GHylomorphism(
  structure F: FUNCTOR
  structure M: MONAD
  structure W: COMONAD
  val distw: 'a W.t F.t -> 'a F.t W.t
  val distm: 'a F.t M.t -> 'a M.t F.t):
sig
  val hylo: ('b W.t F.t -> 'b) -> ('a -> 'a M.t F.t) -> 'a -> 'b
end =
struct
  fun hylo alg coalg =
    let
      fun coalg' x = F.map M.join (distm (M.map coalg x))
      fun alg' x = W.map alg (distw (F.map W.duplicate x))
      fun go x = alg' (F.map go (coalg' x))
    in
      alg o F.map go o coalg
    end
end

functor DistZygo(
  structure F: FUNCTOR
  structure P: PAIR_COMONAD
  val alg: P.left F.t -> P.left):
sig
  val dist: 'a P.t F.t -> 'a F.t P.t
end =
struct
  fun fst (x, _) = x
  fun snd (_, y) = y
  fun dist m = (alg (F.map fst m), F.map snd m)
end
