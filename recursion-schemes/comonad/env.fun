
functor EnvComonad(
  structure W: COMONAD
  type t):
sig
  type e = t

  datatype 'a t = Env of e * 'a W.t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val extract: 'a t -> 'a
  val duplicate: 'a t -> 'a t t

  val ask: 'a t -> e
end =
struct
  type e = t

  datatype 'a t = Env of e * 'a W.t

  fun ask (Env (e, _)) = e

  fun map f (Env (e, w)) = Env (e, W.map f w)

  fun extract (Env (_, w)) = W.extract w

  fun duplicate (Env (e, w)) =
    Env (e, W.map (fn w' => Env (e, w')) (W.duplicate w))
end
