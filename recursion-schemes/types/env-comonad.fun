
functor EnvComonadFn(
  structure W: COMONAD
  type t) :> ENV_COMONAD where type e = t and type 'a W.t = 'a W.t =
struct
  structure W = W
  type e = t
  type 'a t = e * 'a W.t

  fun ask (e, _) = e
  fun lower (_, w) = w
  fun map f (e, w) = (e, W.map f w)
  fun extract (_, w) = W.extract w
  fun duplicate (e, w) = (e, W.map (fn x => (e, x)) (W.duplicate w))
end
