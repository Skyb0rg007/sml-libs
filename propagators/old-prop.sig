
signature PROP =
sig

datatype 'a change =
   Change of 'a
 | NoChange
 | Contradiction of string

structure Cell:
   sig
      type 'a t

      val equals: 'a t * 'a t -> bool
      val new: ('a * 'a -> 'a change) -> 'a t
      val known: ('a * 'a -> 'a change) -> 'a -> 'a t
      val write: 'a t * 'a -> unit
      val watch: 'a t -> ('a -> unit) -> unit
      val watch2: 'a t * 'a t -> ('a * 'a -> unit) -> unit
      val unify: 'a t * 'a t -> unit
      val content: 'a t -> 'a option
      val lift1: ('a -> 'b) -> 'a t * 'b t -> unit
      val lift2: ('a * 'b -> 'c) -> 'a t * 'b t * 'c t -> unit
   end

structure Prop:
   sig
      type 'a t

      val fromInt: int -> int t
      val sign: int t -> int t
      val abs: int t -> int t
      val * : int t * int t -> int t
      val + : int t * int t -> int t
      val arg: 'a Cell.t -> 'a t
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
