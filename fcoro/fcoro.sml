
structure FCoro =
struct

infix /\ \/

structure Push =
   struct
      datatype ('s, 'a) t = T of ('a * 's -> 's) * 's -> 's

      fun cons (x, T fold) = T (fn (c, n) => c (n, fold (c, n)))
   end

structure Point =
   struct
      type t = real * real * real * real

      fun (px, py, pz, pw) /\ (qx, qy, qz, qw) =
         (qx * pw - px * qw,
          qy * pw - py * qw,
          qz * pw - pz * qw,
          py * qz - pz * qy,
          pz * qx - px * qz,
          px * qy - py * qx)
   end

datatype point = Point of {x: real, y: real, z: real}
datatype plane = Plane of {a: real, b: real, c: real, d: real}

type point = real * real
type line = point * point
type plane = real * real * real

val crosses_plane: line -> bool = fn _ => raise Fail "NYI"
val plane_intersect: line -> bool = fn _ => raise Fail "NYI"
val plane: plane = (0.0, 0.0, 0.0)

fun sendCross (pt1, pt2, stream) =
   if crosses_plane (pt1, pt2)
      then Push.cons (SOME (plane_intersect (pt1, pt2)), stream)
   else stream

(* fun sendvispt (pt, stream) = *)
(*    if plane_sign plane pt >= 0 *)
(*       then Push.cons () *)

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
