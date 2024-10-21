
signature PGA =
sig

structure Point:
   sig
      type t

      val x: t -> real
      val y: t -> real
      val z: t -> real
      val w: t -> real

      val fromCoord: real * real * real -> t
      val bulk: t -> t
      val weight: t -> t
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
