
structure Word64 = WordMapFn(
   struct
      open Word64

      val same: word * word -> bool = op =

      fun highestBitMask w =
         let
            infix orb xorb >>

            val w = w orb (w >> 0w1)
            val w = w orb (w >> 0w2)
            val w = w orb (w >> 0w4)
            val w = w orb (w >> 0w8)
            val w = w orb (w >> 0w16)
            val w = w orb (w >> 0w32)
         in
            w xorb (w >> 0w1)
         end
   end)

(* vim: set tw=0 ts=3 sw=3: *)
