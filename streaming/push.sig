
(* Push streams
 * Representation of a lazy right fold
 *
 * Because SML doesn't have universally quanitified types,
 * the fold's state parameter is exposed:
 * - Stream producers always produce maximally polymorphic state types
 * - Stream consumers may constrain the state's type
 *)

signature PUSH =
sig

(* A stream producing elements of type `'a`.
 * `'s` is the state parameter, and is an implementation detail. *)
type ('a, 's) t

(** Construction **)

(* A stream which does not produce any elements *)
val empty: ('a, 's) t

(* A stream which produces the given element before the rest of the stream *)
val cons: 'a * ('a, 's) t -> ('a, 's) t

(* A stream which produces the given element once *)
val singleton: 'a -> ('a, 's) t

(* An infinite stream which continuously produces the given element *)
val repeat: 'a -> ('a, 's) t

(* Produces the elements of the list from right-to-left *)
val fromList: 'a list -> ('a, 's) t

(* Produces the elements of the list from left-to-right *)
val fromListRev: 'a list -> ('a, 's) t

(* Modify elements with a mapping function *)
val map: ('a -> 'b) -> ('a, 's) t -> ('b, 's) t

(* Maps the function over the stream, concatenating the results *)
val concatMap: ('a -> ('b, 's) t) -> ('a, 's) t -> ('b, 's) t

(* Flipped `concatMap` *)
val bind: ('a, 's) t -> ('a -> ('b, 's) t) -> ('b, 's) t

(* Map over two streams *)
val map2: ('a * 'b -> 'c) -> ('a, 's) t * ('b, 's) t -> ('c, 's) t

(* Limit the stream to the first `n` items *)
val take: int -> ('a, int -> 's) t -> ('a, 's) t

(* Skip the first `n` items *)
val drop: int -> ('a, int -> 's) t -> ('a, 's) t

(* Filter *)
val filter: ('a -> bool) -> ('a, 's) t -> ('a, 's) t

(* Append *)
val append: ('a, 's) t * ('a, 's) t -> ('a, 's) t

(** Consumers **)

(* View the first element of the stream *)
val head: ('a, 'a option) t -> 'a option

(* Left fold *)
val foldl: ('a * 'b -> 'b) -> 'b -> ('a, 'b -> 'b) t -> 'b

(* Right fold *)
val foldr: ('a * 'b -> 'b) -> 'b -> ('a, 'b) t -> 'b

(* Collect into a list *)
val toList: ('a, 'a list) t -> 'a list

(* Collect into a list *)
val toListRev: ('a, 'a list -> 'a list) t -> 'a list

(** Conversion **)

(* Create a push stream from a pull stream *)
val fromPull: ('a, 's1) Pull.t -> ('a, 's2) t

(* Create a pull stream from a push stream *)
type 'a stream
val toPull: ('a, 'a stream) t -> ('a, unit -> 'a stream) Pull.t

(** Query **)

val size: ('a, 's) t -> Size.t
val length: ('a, int) t -> int

end

(* vim: set tw=0 ts=3 sw=3: *)
