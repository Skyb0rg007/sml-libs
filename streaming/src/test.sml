
functor TestStream(S: STREAM) =
   struct
      fun range (lo, hi) =
         S.unfold
            (fn x => if x = hi then NONE else SOME (x, x + 1))
            lo

      fun run {limit, length} =
         let
            val s = range (0, length)
            val s = S.map (fn x => x + 1) s
            val s = S.filter (fn x => x mod 3 = 0) s
            val s = S.take limit s
            val s = S.concatMap (fn x => range (x, x + 30)) s
         in
            S.fold op + 0 s
         end
   end

structure Test =
   struct
      structure Seq = TestStream(Seq)
      structure ThunkExn = TestStream(ThunkExn)
      structure PullUnsafe = TestStream(PullUnsafe)
   end

(* vim: set tw=0 ts=3 sw=3: *)
