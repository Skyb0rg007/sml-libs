
signature DYN_ARRAY =
sig

eqtype 'a t

val new: unit -> 'a t
val push: 'a t * 'a -> unit
val app: ('a -> unit) -> 'a t -> unit
val size: 'a t -> int
val sub: 'a t * int -> 'a
val update: 'a t * int * 'a -> unit
val remove: ('a -> bool) -> 'a t -> unit
val findi: (int * 'a -> bool) -> 'a t -> (int * 'a) option
val clear: 'a t -> unit

(* `reserve (arr, n, fill)` ensures the size of the array is at least `n`
 * If this expands the array, uses `fill` for the new slots *)
val reserve: 'a t * int * 'a -> unit

(* `shrinkToFit arr` shrinks the internal allocation to allow for GC *)
val shrinkToFit: 'a t -> unit

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
