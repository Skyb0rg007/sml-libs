
signature PROPAGATOR =
sig

datatype 'a change =
   Unchanged
 | Changed of 'a

structure Scheduler:
   sig
      val enqueue: (unit -> unit) -> unit
      val run: unit -> unit
   end

structure Cell:
   sig
      type 'a t

      val new: 'a -> ('a * 'a -> 'a change) -> 'a t
      val content: 'a t -> 'a
      val write: 'a t -> 'a -> unit
      val watch: 'a t -> ('a -> unit) -> unit
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
