
signature MONOID =
   sig
      type t

      val zero: t
      val + : t * t -> t
   end

signature FINGER_TREE_STRUCTS =
   sig
      structure Measure: MONOID

      structure Item:
         sig
            type 'a t

            val measure: 'a t -> Measure.t
         end
   end

signature FINGER_TREE =
   sig
      include FINGER_TREE_STRUCTS

      structure Lazy: LAZY

      type 'a t

      (** Creation **)
      (* O(1) Empty sequence *)
      val empty: 'a t
      (* O(1) Singleton sequence *)
      val singleton: 'a Item.t -> 'a t
      (* O(1) Add an element to the left end *)
      val cons: 'a Item.t * 'a t -> 'a t
      (* O(1) Add an element to the right end *)
      val snoc: 'a t * 'a Item.t -> 'a t
      (* O(log(min(n, m))) Concatenate two sequences *)
      val append: 'a t * 'a t -> 'a t

      (** Destruction **)
      (* O(1) Analyze the left end of the sequence *)
      val viewl: 'a t -> ('a Item.t * 'a t Lazy.t) option
      (* O(1) Analyze the right end of the sequence *)
      val viewr: 'a t -> ('a t Lazy.t * 'a Item.t) option

      (** Maps and Folds (all O(n)) **)
      val map: ('a Item.t -> 'b Item.t) -> 'a t -> 'b t
      val mapWithPos: (Measure.t * 'a Item.t -> 'b Item.t) -> 'a t -> 'b t
      val mapWithContext: (Measure.t * 'a Item.t * Measure.t -> 'b Item.t) -> 'a t -> 'b t
      val foldl: ('a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldlWithPos: (Measure.t * 'a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldlWithContext: (Measure.t * 'a Item.t * Measure.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldr: ('a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldrWithPos: (Measure.t * 'a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldrWithContext: (Measure.t * 'a Item.t * Measure.t * 'b -> 'b) -> 'b -> 'a t -> 'b

      (** Conversion (both O(n)) **)
      val toList: 'a t -> 'a Item.t list
      val fromList: 'a Item.t list -> 'a t

      (** Operations **)
      (* O(1) Return the cached measure *)
      val measure: 'a t -> Measure.t
      (* O(1) Is the sequence empty? *)
      val isEmpty: 'a t -> bool
      (* O(n) Remove all thunks from the data structure *)
      val force: 'a t -> unit
      (* O(n) Reverse the sequence *)
      val reverse: 'a t -> 'a t
      (* val debugShow: (Measure.t -> string) -> ('a Item.t -> string) -> 'a t -> string *)

      (** Search/Split **)
      datatype 'a search_result =
         Position of 'a t * 'a Item.t * 'a t
       | OnLeft
       | OnRight
       | Nowhere

      (* O(log(min(i, n - i))) Search for a point where predicate changes from false to true *)
      val search: (Measure.t * Measure.t -> bool) -> 'a t -> 'a search_result
      (* O(log(min(i, n - i))) Split the sequence at the point where predicate changes from false to true *)
      val split: (Measure.t -> bool) -> 'a t -> 'a t * 'a t

      val debugShow: ('a -> string) -> 'a t -> string
   end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
