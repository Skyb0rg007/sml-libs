
signature WEAK_BAG =
sig

type 'a t
type 'a ticket

val new: unit -> 'a t
val isEmpty: 'a t -> bool
val insert: 'a t * 'a -> 'a ticket
val app: ('a -> unit) -> 'a t -> unit
val remove: 'a ticket -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)
