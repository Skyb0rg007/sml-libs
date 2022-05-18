
structure Compat =
struct
   structure Vector =
      struct
         open Vector

         fun unfoldi (n, z, f) =
            let
               val arr = Array.array (n, NONE)
               fun loop (i, acc) =
                  if i >= n
                     then acc
                  else
                     let
                        val (x, acc) = f (i, acc)
                     in
                        Array.update (arr, i, SOME x)
                        ; loop (i + 1, acc)
                     end
               val acc = loop (0, z)
               fun gen i = Option.valOf (Array.sub (arr, i))
            in
               (Vector.tabulate (n, gen), acc)
            end
      end

   structure Word32 =
      struct
         open Word32
      end
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
