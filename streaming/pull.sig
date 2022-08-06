
(* Pull-based streams (think C++ iterators)
 * Supports efficient `foldl` and `zip`
 * Doesn't support efficient concatenation or bind
 *)

signature PULL =
sig

type ('a, 's) t

(* Creation *)
val empty: ('a, unit) t
val repeat: 'a -> ('a, unit) t
val range: int * int -> (int, int) t
val countFrom: int -> (int, int) t
val fromVector: 'a vector -> ('a, int) t
val fromList: 'a list -> ('a, 'a list) t
val unfold: ('b -> ('a * 'b) option) -> 'b -> ('a, 'b) t

(* Operation *)
val map: ('a -> 'b) -> ('a, 's) t -> ('b, 's) t
val filter: ('a -> bool) -> ('a, 's) t -> ('a, 's) t
val take: int -> ('a, 's) t -> ('a, 's * int) t
val drop: int -> ('a, 's) t -> ('a, 's) t
val zipWith: ('a * 'b -> 'c) -> ('a, 's1) t * ('b, 's2) t -> ('c, 's1 * 's2 * 'a option) t
val zipWith3: ('a * 'b * 'c -> 'd) -> ('a, 's1) t * ('b, 's2) t * ('c, 's3) t -> ('d, 's1 * 's2 * 's3 * ('a * 'b option) option) t

(* Elimination *)
(* Note: These functions don't terminate on infinite streams *)
val foldl: ('a * 'b -> 'b) -> 'b -> ('a, 's) t -> 'b
val foldr: ('a * 'b -> 'b) -> 'b -> ('a, 's) t -> 'b
val length: ('a, 's) t -> int
val toList: ('a, 's) t -> 'a list
val toVector: ('a, 's) t -> 'a vector
val toArray: ('a, 's) t -> 'a array

(* May not terminate on filtered infinite streams
 * Ex. `uncons (filter (fn _ => false) (repeat x))` doesn't terminate *)
val uncons: ('a, 's) t -> ('a * ('a, 's) t) option

(* Constant-time size estimation *)
val size: ('a, 's) t -> Size.t
(* Add a size annotation
 * Note: Must be correct! *)
val unsafeSized: ('a, 's) t * Size.t -> ('a, 's) t

end

(* vim: set tw=0 ts=3 sw=3: *)
