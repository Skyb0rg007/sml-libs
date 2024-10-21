
signature DEQUE =
sig

type 'a t

exception Empty

val empty : 'a t
val isEmpty : 'a t -> bool

val cons : 'a * 'a t -> 'a t
val head : 'a t -> 'a
val tail : 'a t -> 'a t

val snoc : 'a t * 'a -> 'a t
val last : 'a t -> 'a
val init : 'a t -> 'a t

val toList : 'a t -> 'a list

end

(* vim: set ts=3 sw=3: *)
