
signature SEXP_NUMERIC_TOWER =
sig

datatype t =
   FIX of int
 | BIG of IntInf.int
 | RAT of t * t
 | FLO of real
 | CPX of t * t

val fix: int -> t
val big: IntInf.int -> t
val rat: t * t -> t
val flo: real -> t
val cpx: t * t -> t

val + : t * t -> t

val valid: t -> bool
val toString: t -> string
(* Total function, `same (NaN, NaN) = true` *)
val same: t * t -> bool
(* Raises Domain on complex input, raises IEEEReal.Unordered on NaN *)
val compare: t * t -> order

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
