
(* Polymorphic ordered set *)

signature ORD_SET =
sig

type 'a t

val empty: ('a * 'a -> order) -> 'a t
val insert: 'a t * 'a -> 'a t
val member: 'a t * 'a -> bool

end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
