
functor IntervalMap(Key: ORD_KEY) =
struct
   structure Measure =
      struct
         type t = (Key.ord_key * Key.ord_key * Key.ord_key) option

         val zero = NONE
         fun NONE + i = i
           | i + NONE = i
           | (SOME (_, _, hi)) + (SOME (a, b, hi')) =
            case Key.compare (hi, hi') of
               LESS => SOME (a, b, hi')
             | _ => SOME (a, b, hi)
      end

   structure Item =
      struct
         type 'a t = Key.ord_key * Key.ord_key * 'a

         fun measure (lo, hi, _) = SOME (lo, hi, hi)
      end

   structure T = FingerTree(
      struct
         structure Measure = Measure
         structure Item = Item
      end)

   (* TODO : https://hackage.haskell.org/package/fingertree-0.1.5.0/docs/Data-IntervalMap-FingerTree.html *)
end

(* vim: set ft=sml ts=3 sw=3 tw=0: *)
