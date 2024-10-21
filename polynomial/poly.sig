
signature POLY =
sig

structure Coeff:
   sig
      type t
   end

type t

(* val quot: t * t -> t *)
(* val rem: t * t -> t *)
(* val divide: t * t -> t option *)
(* val gcd: t * t -> t *)
(* val lcm: t * t -> t *)
(* val coprime: t * t -> bool *)
val + : t * t -> t
val - : t * t -> t
val * : t * t -> t
val ~ : t -> t
val constant: Coeff.t -> t
val evaluate: t * Coeff.t -> Coeff.t
(* val derivative: t -> t *)
(* val integral: t -> t *)
val var: t
val isVar: t -> bool

end

(* vim: set tw=0 ts=3 sw=3: *)
