(* Indication of stream size *)

signature SIZE =
sig

type t

(* `fromInt n`: exactly `n` items *)
val fromInt: int -> t

(* `unknown`: an unknown (possibly infinite) number of items *)
val unknown: t

(* `toMax sz`: any number up to `sz` items *)
val toMax: t -> t

(* `max (s1, s2)`: the larger of the two bounds *)
val max: t * t -> t

(* `min (s1, s2)`: the smaller of the two bounds *)
val min: t * t -> t

(* `s1 + s2`: the combined sizes of the two bounds *)
val + : t * t -> t

(* `s1 - s2`: the difference of the two bounds (saturated at 0) *)
val - : t * t -> t

(* The smallest number of items the stream produces *)
val lowerBound: t -> int

(* The largest number of items the stream produces, or NONE if unbounded *)
val upperBound: t -> int option

(* The exact number of items the stream produces, or NONE if inexact *)
val exact: t -> int option

end
(* vim: set tw=0 ts=3 sw=3: *)
