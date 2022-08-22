
signature WORD_MAP =
sig

include MAP

val viewMin: 'a map -> (key * 'a * 'a map) option
val viewMax: 'a map -> (key * 'a * 'a map) option
val findMin: 'a map -> (key * 'a) option
val findMax: 'a map -> (key * 'a) option
val deleteMin: 'a map -> 'a map option
val deleteMax: 'a map -> 'a map option

structure Fold:
   sig
      structure Base:
         sig
            datatype ('a, 'b) t =
               NilF
             | TipF of key * 'b
             | BinF of 'a * 'a

            val map: ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
            val project: 'a map -> ('a map, 'a) t
         end

      structure Cofree:
         sig
            datatype ('a, 'b) t = :< of 'a * (('a, 'b) t, 'b) Base.t

            val map: ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
            val extract: ('a, 'b) t -> 'a
            val unwrap: ('a, 'b) t -> (('a, 'b) t, 'b) Base.t
            val duplicate: ('a, 'b) t -> (('a, 'b) t, 'b) t
            val extend: (('a, 'c) t -> 'b) -> ('a, 'c) t -> ('b, 'c) t
         end

      val cata: (('a, 'b) Base.t -> 'a) -> 'b map -> 'a
      val para: (('b map * 'a, 'b) Base.t -> 'a) -> 'b map -> 'a
      val histo: ((('a, 'b) Cofree.t, 'b) Base.t -> 'a) -> 'b map -> 'a
      val zygo: (('b, 'c) Base.t -> 'b) -> (('b * 'a, 'c) Base.t -> 'a) -> 'c map -> 'a
   end

structure Zipper:
   sig
      type 'a t

      val fromMap: 'a map -> 'a t
      val toMap: 'a t -> 'a map

      val up: 'a t -> 'a t option
      val downLeft: 'a t -> 'a t option
      val downRight: 'a t -> 'a t option

      val focus: 'a t -> 'a map
      val filterFocus: ('a -> bool) -> 'a t -> 'a t
      val mapFocus: ('a -> 'a) -> 'a t -> 'a t
      val mapPartialFocus: ('a -> 'a option) -> 'a t -> 'a t
      val differenceFocus: 'a t * 'a map -> 'a t

      val tug: ('a -> 'a option) -> 'a -> 'a
      val tugs: ('a -> 'a option) -> int -> 'a -> 'a
      val jerks: ('a -> 'a option) -> int -> 'a -> 'a option
      val furthest: ('a -> 'a option) -> 'a -> 'a
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
