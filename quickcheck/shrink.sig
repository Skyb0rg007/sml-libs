
signature SHRINK_NUMBER =
sig
   type t

   val compare: t * t -> order
   val div: t * t -> t
   val + : t * t -> t
   val - : t * t -> t
   val fromInt: int -> t
end

signature SHRINK =
sig
   type t

   (* Shrinks towards `dest` *)
   val shrink: {dest: t} -> t -> t Seq.t

   (* Shrinks towards `dest` faster *)
   val shrinkAggressive: {dest: t} -> t -> t Seq.t
end
(* vim: set tw=0 ts=3 sw=3: *)
