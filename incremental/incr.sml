
structure Incr:
   sig
      (* type 'a t *)

      (* val pure: 'a -> 'a t *)
      (* val map: ('a -> 'b) -> 'a t -> 'b t *)
      (* val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t *)
      (* val bind: 'a t -> ('a -> 'b t) -> 'b t *)

      (* val stabilize: unit -> unit *)
      (* val value: 'a t -> 'a *)
      (* val onUpdate: 'a t * ('a -> unit) -> unit *)
   end =
   struct
   end

structure Var:
   sig
      (* type 'a t *)

      (* val new: 'a -> 'a t *)
      (* val set: 'a t * 'a -> unit *)
      (* val read: 'a t -> 'a Incr.t *)
   end =
   struct
   end

(* vim: set tw=0 ts=3 sw=3: *)
