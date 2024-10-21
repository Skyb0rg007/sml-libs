
signature HEAP =
sig

type t
type elt

exception Empty

val empty : t
val isEmpty : t -> bool
val insert : t * elt -> t
val merge : t * t -> t
val findMin : t -> elt
val deleteMin : t -> t

end

(* vim: set ts=3 sw=3: *)
