
structure Compat =
struct
   structure Fn =
      struct
         fun flip f (x, y) = f (y, x)
      end

   structure Vector =
      struct
         open Vector

         fun append (v, x) =
            let
               val n = length v
               fun gen i =
                  if i = n
                     then x
                  else sub (v, i)
            in
               tabulate (n, gen)
            end

         fun update (v, i, x) =
            let
               fun gen j =
                  if j = i
                     then x
                  else sub (v, j)
            in
               tabulate (length v, gen)
            end

         fun toList v = foldr op:: [] v

         val unfoldi = MLton.Vector.unfoldi
      end

   structure Word32 =
      struct
         open Word32

         fun popCount w =
            let
               infix >> andb
               val w = w - ((w >> 0w1) andb 0wx55555555)
               val w = (w andb 0wx33333333) + ((w >> 0w2) andb 0wx33333333)
            in
               Word32.toInt ((((w + (w >> 0w4)) andb 0wxf0f0f0f) * 0wx1010101) >> 0w24)
            end
      end
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
