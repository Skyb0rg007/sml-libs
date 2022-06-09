(* Datatype and combinators for short-circuiting left folds *)

signature FOLDL =
sig
   (* Main datatype
    * 's is the state parameter; it should be ignored and considered an implementation detail
    * 'a is the input parameter
    * 'b is the output parameter
    *
    * if step returns `Either.INL b`, then the fold short-circuits with result `b`
    * otherwise the fold continues to consume input
    *)
   datatype ('s, 'a, 'b) t = T of {
      init: 's,
      step: 'a * 's -> ('b, 's) Either.either,
      done: 's -> 'b
   }

   (* Implementation details, you should ignore these *)
   (* Unfortunately SML doesn't support existential types so these must be exposed *)
   type ('s, 't, 'a, 'b) map_state
   type ('s, 't, 'b) o_state
   type 'a ord_set

   (* Applying a fold *)
   val foldList: ('s, 'a, 'b) t -> 'a list -> 'b
   val foldOption: ('s, 'a, 'b) t -> 'a option -> 'b
   val foldVector: ('s, 'a, 'b) t -> 'a vector -> 'b
   val foldArray: ('s, 'a, 'b) t -> 'a array -> 'b
   val foldGetc: ('s, 'a, 'b) t -> (unit -> 'a option) -> 'b
   val foldTextIO: ('s, char, 'b) t -> TextIO.instream -> 'b
   val foldTextIOLine: ('s, string, 'b) t -> TextIO.instream -> 'b
   val foldBinIO: ('s, Word8.word, 'b) t -> BinIO.instream -> 'b

   (* Combinators *)

   (* Precompose a function *)
   val map: ('b -> 'c) -> ('s, 'a, 'b) t -> ('s, 'a, 'c) t
   (* Postcompose a function *)
   val lmap: ('a -> 'b) -> ('s, 'b, 'c) t -> ('s, 'a, 'c) t
   (* Pre and post compose functions *)
   val dimap: ('a -> 'b) -> ('c -> 'd) -> ('s, 'b, 'c) t -> ('s, 'a, 'd) t
   (* Allow state to pass through unchanged *)
   val left: ('s, 'a, 'b) t -> ('s, ('a, 'c) Either.either, ('b, 'c) Either.either) t
   val right: ('s, 'a, 'b) t -> ('s, ('c, 'a) Either.either, ('c, 'b) Either.either) t
   (* Immediately return with a value *)
   val pure: 'b -> (unit, 'a, 'b) t
   (* Map over the result of two folds *)
   val map2: ('b * 'c -> 'd) -> ('s, 'a, 'b) t * ('t, 'a, 'c) t -> (('s, 't, 'b, 'c) map_state, 'a, 'd) t
   (* Run two folds, applying the result of one to the result of the other *)
   val ap: ('s, 'a, 'b -> 'c) t * ('t, 'a, 'b) t -> (('s, 't, 'b -> 'c, 'b) map_state, 'a, 'c) t
   (* Run a fold on the empty input *)
   val extract: ('s, 'a, 'b) t -> 'b
   (* Run a fold, returning a fold initialized with the result of the first *)
   val duplicate: ('s, 'a, 'b) t -> ('s, 'a, ('s, 'a, 'b) t) t
   val extend: ('s, 'a, 'b) t -> (('s, 'a, 'b) t -> 'c) -> ('s, 'a, 'c) t
   (* Run the first fold on the partial results of the second *)
   val o: ('s, 'b, 'c) t * ('t, 'a, 'b) t -> (('s, 't, 'b) o_state, 'a, 'c) t
   (* Filter the inputs to a fold *)
   val filter: ('a -> bool) -> ('s, 'a, 'b) t -> ('s, 'a, 'b) t
   (* Skip inputs to a fold while the predicate holds *)
   val dropWhile: ('a -> bool) -> ('s, 'a, 'b) t -> (bool * 's, 'a, 'b) t
   (* Skip the first n inputs *)
   val drop: int -> ('s, 'a, 'b) t -> (int * 's, 'a, 'b) t
   (* Thread the input to one of the two folds, returning both results at the end *)
   val either: ('s, 'a1, 'b1) t * ('t, 'a2, 'b2) t -> (('s, 't, 'b1, 'b2) map_state, ('a1, 'a2) Either.either, 'b1 * 'b2) t

   (* Common folds *)
   val length: (int, 'a, int) t
   val sum: (int, int, int) t
   val product: (int, int, int) t
   val head: (unit, 'a, 'a option) t
   val last: ('a option, 'a, 'a option) t
   val null: (unit, 'a, bool) t
   val all: ('a -> bool) -> (unit, 'a, bool) t
   val any: ('a -> bool) -> (unit, 'a, bool) t
   val list: ('a list -> 'a list, 'a, 'a list) t
   val revList: ('a list, 'a, 'a list) t
   val reduce: ('a * 'a -> 'a) -> ('a option, 'a, 'a option) t
   (* Remove duplicates (O(n log n)) *)
   val nub: ('a * 'a -> order) -> ('a ord_set * ('a list -> 'a list), 'a, 'a list) t
   (* Remove duplicates (O(n^2)). Use `nub` instead if possible. *)
   val eqNub: ('a * 'a -> bool) -> ('a list * ('a list -> 'a list), 'a, 'a list) t
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
