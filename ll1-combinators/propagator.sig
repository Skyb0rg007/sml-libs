
signature PROPAGATOR =
sig

exception Inconsistent

(* false < true *)
structure Bool:
   sig
      type t
      type cell

      val true_: t
      val false_: t
      val cell: cell -> t

      val newCell: unit -> cell
      val setCell: cell * t -> unit

      val toBool: t -> bool
      val and_: t * t -> t
      val or: t * t -> t
   end

(* NONE < SOME x < Inconsistent *)
structure Option:
   sig
      type 'a t
      type 'a cell

      val none: 'a t
      val some: 'a -> 'a t
      val cell: 'a cell -> 'a t

      val newCell: unit -> 'a cell
      val setCell: 'a cell * 'a t -> unit

      val toOption: 'a t -> 'a option
      val inconsistent: 'a t -> bool

      (* or (NONE, NONE) = NONE
       * or (SOME x, NONE) = SOME x
       * or (NONE, SOME y) = SOME y
       * or (_, _) = TOP
       *)
      val or: 'a t * 'a t -> 'a t

      (* map _ NONE = NONE
       * map f (SOME x) = SOME (f x)
       * map _ TOP = TOP
       *)
      val map: ('a -> 'b) -> 'a t -> 'b t

      (* map2 _ (NONE, NONE) = NONE
       * map2 _ (NONE, SOME _) = NONE
       * map2 _ (SOME _, NONE) = NONE
       * map2 f (SOME x, SOME y) = SOME (f (x, y))
       * map2 _ (_, _) = TOP
       *)
      val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t

      val isSome: 'a t -> Bool.t
   end

structure Set:
   sig
      type t
      type cell

      val empty: t
      val singleton: int -> t
      val cell: cell -> t

      val toSet: t -> IntRedBlackSet.set
      val inconsistent: t -> bool

      val newCell: unit -> cell
      val setCell: cell * t -> unit

      val union: t * t -> t
      (* guard (false, _) = empty
       * guard (true, s) = s
       *)
      val guard: Bool.t * t -> t
   end

end

(* vim: set tw=0 sw=3 ts=3: *)
