
signature NODE' =
sig

type 'a t

val stabilize: unit -> unit

structure Observer:
   sig
      type t

      val new: (unit -> unit) -> t
      val call: t -> unit
      val equals: t * t -> bool
   end

val constant: 'a -> 'a t
val bind: 'a t -> ('a -> 'b t) -> 'b t
val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b t
val leftmost: 'a t list -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val map2: ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
val mapOptional: ('a -> 'b option) -> 'a t -> 'b t
val switch: {alwaysFire: bool} -> 'a t -> ('a -> 'b t) -> 'b t

val addObserver: 'a t * Observer.t -> unit
val removeObserver: 'a t * Observer.t -> unit

type 'a event
val newEvent: unit -> 'a event
val readEvent: 'a event -> 'a t
val triggerEvent: 'a event * 'a -> unit

type 'a var
val newVar: 'a -> 'a var
val readVar: 'a var -> 'a t
val setVar: 'a var * 'a -> unit
(* sample *)

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
