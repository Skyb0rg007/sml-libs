
structure SLf =
struct

structure Box =
   struct
      datatype 'a t = T of int * 'a

      val counter = ref 0

      fun new x = T (!counter, x) before counter := !counter + 1

      fun valOf (T (_, x)) = x

      fun indexOf (T (k, _)) = k

      fun equals (T (k, _), T (k', _)) = k = k'
   end



end

(* vim: set tw=0 sw=3 ts=3: *)
