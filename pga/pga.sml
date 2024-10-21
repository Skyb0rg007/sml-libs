
structure PGA: PGA =
struct

structure Point =
   struct
      datatype t = T of real * real * real * real

      fun x (T (x, _, _, _)) = x
      fun y (T (_, y, _, _)) = y
      fun z (T (_, _, z, _)) = z
      fun w (T (_, _, _, w)) = w

      fun fromCoord (x, y, z) = T (x, y, z, 1.0)
      fun bulk (T (x, y, z, _)) = T (x, y, z, 0.0)
      fun weight (T (_, _, _, w)) = T (0.0, 0.0, 0.0, w)
   end

structure Line =
   struct
      datatype t = T of real * real * real * real * real * real
   end

structure Plane =
   struct
      datatype t = T of real * real * real * real
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
