
signature ISO =
sig

type ('a, 'b) t

val invert: ('a, 'b) t -> ('b, 'a) t
val inject: ('a, 'b) t * 'a -> 'b
val project: ('a, 'b) t * 'b -> 'a

val make: ('a -> 'b) -> ('b -> 'a) -> ('a, 'b) t
val fromTag: 'a Universal.tag -> ('a, Universal.universal) t

end

(* vim: set tw=0 ts=3 sw=3: *)
