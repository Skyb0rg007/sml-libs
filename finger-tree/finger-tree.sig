
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

      type 'a t

      (* Creation *)
      val empty: 'a t
      val singleton: 'a Item.t -> 'a t
      val cons: 'a Item.t * 'a t -> 'a t
      val snoc: 'a t * 'a Item.t -> 'a t
      val append: 'a t * 'a t -> 'a t

      (* Destruction *)
      val viewl: 'a t -> ('a Item.t * 'a t Lazy.t) option
      val viewr: 'a t -> ('a t Lazy.t * 'a Item.t) option

      (* Maps and Folds *)
      val map: ('a Item.t -> 'b Item.t) -> 'a t -> 'b t
      val mapWithPos: (Measure.t * 'a Item.t -> 'b Item.t) -> 'a t -> 'b t
      val mapWithContext: (Measure.t * 'a Item.t * Measure.t -> 'b Item.t) -> 'a t -> 'b t
      val foldl: ('a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldlWithPos: (Measure.t * 'a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldlWithContext: (Measure.t * 'a Item.t * Measure.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldr: ('a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldrWithPos: (Measure.t * 'a Item.t * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldrWithContext: (Measure.t * 'a Item.t * Measure.t * 'b -> 'b) -> 'b -> 'a t -> 'b

      (* Conversion *)
      val toList: 'a t -> 'a Item.t list
      val fromList: 'a Item.t list -> 'a t

      (* Operations *)
      val measure: 'a t -> Measure.t
      val isEmpty: 'a t -> bool
      val force: 'a t -> unit
      val reverse: 'a t -> 'a t
      (* val debugShow: (Measure.t -> string) -> ('a Item.t -> string) -> 'a t -> string *)

      (* Search/Split *)
      datatype 'a search_result =
         Position of 'a t * 'a Item.t * 'a t
       | OnLeft
       | OnRight
       | Nowhere

      val search: (Measure.t * Measure.t -> bool) -> 'a t -> 'a search_result
      val split: (Measure.t -> bool) -> 'a t -> 'a t * 'a t
   end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
