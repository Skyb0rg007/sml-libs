
signature PROPOGATE =
sig

type 'a t
type s

val alloc: unit -> 'a t
val write: 'a t * 'a -> unit
val read: 'a t list * ('a list -> unit) -> unit
val par: (unit -> unit) * (unit -> unit) -> unit
val run: (unit -> unit) -> s
val propogate: s -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)
