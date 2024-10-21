
signature CONDUIT =
sig

type 'a m
type ('i, 'o, 'a) t

val pure: 'a -> ('i, 'o, 'a) t
val map: ('a -> 'b) -> ('i, 'o, 'a) t -> ('i, 'o, 'b) t
val >>= : ('i, 'o, 'a) t * ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t
val >=> : ('a -> ('i, 'o, 'b) t) * ('b -> ('i, 'o, 'c) t) -> 'a -> ('i, 'o, 'c) t

val lift: 'a m -> ('i, 'o, 'a) t
val await: ('i, 'o, 'i option) t
val awaitForever: ('i -> ('i, 'o, 'r) t) -> ('i, 'o, unit) t
val yield: 'o -> ('i, 'o, unit) t
val yieldM: 'o m -> ('i, 'o, unit) t
val leftover: 'i -> ('i, 'o, unit) t
val |> : ('a, 'b, unit) t * ('b, 'c, 'r) t -> ('a, 'c, 'r) t
val run: (unit, Void.t, 'r) t -> 'r m

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
