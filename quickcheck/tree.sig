
signature TREE =
sig
   type 'a t

   val make: 'a * 'a t Seq.t -> 'a t
   val root: 'a t -> 'a
   val children: 'a t -> 'a t Seq.t
   val map: ('a -> 'b) -> 'a t -> 'b t
   val ap: ('a -> 'b) t * 'a t -> 'b t
   val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
   val bind: 'a t -> ('a -> 'b t) -> 'b t
   val pure: 'a -> 'a t
   val opt: 'a t -> 'a option t
   val sequence: 'a t list -> 'a list t
   val filterShrinks: ('a -> bool) -> 'a t -> 'a t
   val applicativeTake: int * 'a t list -> 'a list t
   val makePrimitive: ('a -> 'a Seq.t) -> 'a -> 'a t
end

(* vim: set tw=0 ts=3 sw=3: *)
