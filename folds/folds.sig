
signature FOLDS =
sig
   type x

   datatype ('a, 'b) t = Fold of {
      step: 'a * x -> x,
      init: x,
      done: x -> 'b
   }

   val make: ('a * 'x -> 'x) -> 'x -> ('x -> 'b) -> ('a, 'b) t

   val foldList: ('a, 'b) t -> 'a list -> 'b


   (* Combinators *)

   (* Functor *)
   val map: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

   (* Profunctor *)
   val lmap: ('c -> 'a) -> ('a, 'b) t -> ('c, 'b) t

   (* Choice Profunctor *)
   val right: ('a, 'b) t -> (('c, 'a) Either.either, ('c, 'b) Either.either) t
   val left: ('a, 'b) t -> (('a, 'c) Either.either, ('b, 'c) Either.either) t

   (* Applicative *)
   val pure: 'b -> ('a, 'b) t
   val ap: ('a, 'b -> 'c) t * ('a, 'b) t -> ('a, 'c) t
   val map2: ('b * 'c -> 'd) -> ('a, 'b) t * ('a, 'c) t -> ('a, 'd) t

   (* Comonad *)
   val extract: ('a, 'b) t -> 'b
   val duplicate: ('a, 'b) t -> ('a, ('a, 'b) t) t
   val extend: ('a, 'b) t -> (('a, 'b) t -> 'c) -> ('a, 'c) t

   (* Semigroupoid *)
   val o: ('b, 'c) t * ('a, 'b) t -> ('a, 'c) t

   (* Simple folds *)
   val head: unit -> ('a, 'a option) t
   val last: unit -> ('a, 'a option) t
   val lastN: int -> ('a, 'a list) t
   val null: ('a, bool) t

   (* Common folds *)
   val sum: (int, int) t
   val length: ('a, int) t
end
(* vim: set ft=sml ts=3 sw=3 tw=0: *)
