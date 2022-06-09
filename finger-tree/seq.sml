
structure Seq = FingerTree(
   struct
      structure Measure =
         struct
            type t = int
            val op + = Int.+
            val zero = 0
         end

      structure Item =
         struct
            type 'a t = 'a
            fun measure _ = 1
         end
   end)

structure Seq =
   struct
      open Seq

      val length = measure

      fun splitAt n = split (fn i => i > n)
   end

(* vim: set ft=sml ts=3 sw=3 tw=0: *)
