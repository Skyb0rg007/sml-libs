
signature ENV_COMONAD =
sig
  structure W: COMONAD

  type e

  type 'a t = e * 'a W.t

  (* ComonadEnv *)
  val ask: 'a t -> e
  (* ComonadTrans *)
  val lower: 'a t -> 'a W.t
  (* Comonad *)
  val map: ('a -> 'b) -> 'a t -> 'b t
  val extract: 'a t -> 'a
  val duplicate: 'a t -> 'a t t
end
