
signature FRP =
sig

structure Event:
   sig
      type 'a t

      val never: 'a t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val filter: ('a -> bool) -> 'a t -> 'a t
      val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
      val leftmost: 'a t list -> 'a t

      val subscribe: 'a t -> ('a -> unit) -> unit
      val new: unit -> 'a t * ('a -> unit)
   end

structure Dynamic:
   sig
      type 'a t

      val pure: 'a -> 'a t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
      val ap: ('a -> 'b) t * 'a t -> 'b t
      val >>= : 'a t * ('a -> 'b t) -> 'b t
      val switch: 'a Event.t t -> 'a Event.t
   end

val cleanup: unit -> unit

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
