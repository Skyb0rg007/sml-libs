
signature EVENT =
sig

type 'a t
type cancellation

val map: ('a -> 'b) -> 'a t -> 'b t
val map2: ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
val compact: 'a option t -> 'a t
val separate: ('a, 'b) Either.either t -> 'a t * 'b t
val filter: ('a -> bool) -> 'a t -> 'a t
val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
val partitionEither: ('a -> ('b, 'c) Either.either) -> 'a t -> 'b t * 'c t
val apply: ('a -> 'b) t * 'a t -> 'b t
val pure: 'a -> 'a t
val alt: 'a t * 'a t -> 'a t
val empty: 'a t
val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b t
val sampleOn: 'a t * ('a -> 'b) t -> 'b t
val keepLatest: 'a t t -> 'a t
val fix: ('a t -> {input: 'a t, output: 'b t}) -> 'b t
val subscribe: 'a t -> ('a -> unit) -> cancellation
val create: unit -> {event: 'a t, push: 'a -> unit}
val count: 'a t -> int t
val withLast: 'a t -> {now: 'a, last: 'a option} t
val mapAccum: ('a * 'b -> 'b * 'c) -> 'b -> 'a t -> 'c t
val gate: bool t * 'a t -> 'a t

val cancel: cancellation -> unit

end

(* vim: set tw=0 ts=3 sw=3: *)
