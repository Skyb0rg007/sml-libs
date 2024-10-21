
structure Profile =
struct

structure Data =
   struct
      datatype t = T of unit ref

      fun equals (T a, T b) = a = b
      fun malloc () = T (ref ())
      fun free (T _) = ()
      fun write (T _, _: string) = ()
   end

val isOn = false
fun withData (Data.T _, f) = f ()

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
