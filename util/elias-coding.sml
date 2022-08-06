
structure EliasCoding =
struct

   local
      val tbl = #[0,5,1,6,4,3,2,7]
      val orb = Word8.orb
      val >> = Word8.>>
      infix orb >>
   in
      fun ilog2 w =
         let
            val w = w orb (w >> 0w1)
            val w = w orb (w >> 0w2)
            val w = w orb (w >> 0w4)
         in
            Vector.sub (tbl, Word8.toInt ((0wx1d * w) >> 0w5))
         end
   end

   structure Gamma =
   struct
   end

   structure Delta =
   struct
      fun encode (input, output) =
         case BinIO.input1 input of
            NONE => ()
          | SOME w =>
               let
               in
                  ()
               end
   end
end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
