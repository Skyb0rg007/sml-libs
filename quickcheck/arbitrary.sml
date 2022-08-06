
structure Arbitrary: ARBITRARY =
struct

structure Gen =
   struct
      type 'a t = SplitMix.t * int -> 'a

      fun pure x _ = x

      fun map f x s = f (x s)

      fun map2 f (x, y) (g, n) =
         let
            val (g1, g2) = SplitMix.split g
         in
            f (x (g1, n), y (g2, n))
         end

      fun map3 f (x, y, z) (g, n) =
         let
            val (g1, g') = SplitMix.split g
            val (g2, g3) = SplitMix.split g'
         in
            f (x (g1, n), y (g2, n), z (g3, n))
         end

      fun bind x f (g, n) =
         let
            val (g1, g2) = SplitMix.split g
         in
            f (x (g1, n)) (g2, n)
         end

      fun sequence [] = pure []
        | sequence (g :: gs) = bind g (fn x => map (fn xs => x :: xs) (sequence gs))

      fun fix f =
         let
            fun f' x s = f f' x s
         in
            f'
         end

      fun delay (g, n) f = f (g, n)

      fun promote m = bind delay (fn eval => pure (eval o m))

      fun variant (k, f) (g, n) =
         let
            val left = #1 o SplitMix.split
            val right = #2 o SplitMix.split

            fun ilog2 1 = 0
              | ilog2 n = 1 + ilog2 (n div 2)

            fun gamma (n, g) =
               let
                  val k = ilog2 n

                  fun testBit m = IntInf.andb (n, IntInf.<< (1, Word.fromInt m)) <> 0

                  fun encode (~1, g) = g
                    | encode (k, g) =
                     if testBit k
                        then encode (k - 1, right g)
                     else encode (k - 1, left g)

                  fun zeroes (0, g) = g
                    | zeroes (k, g) = zeroes (k - 1, left g)
               in
                  encode (k, zeroes (k, g))
               end

            fun go (n, g) =
               if n >= 1
                  then gamma (n, left g)
               else gamma (1 - n, right g)
         in
            f (go (k, g), n)
         end

      fun sized f (g, n) = f n (g, n)

      fun resize (n, f) (g, _) = f (g, n)

      fun scale h f (g, n) = f (g, h n)

      fun chooseInt (lo, hi) (g, _) =
         #1 (SplitMix.chooseInt (lo, hi) g)

      fun chooseWord (lo, hi) (g, _) =
         #1 (SplitMix.chooseWord (lo, hi) g)

      fun chooseWord64 (lo, hi) (g, _) =
         #1 (SplitMix.chooseWord64 (lo, hi) g)

      fun chooseIntInf (lo, hi) (g, _) =
         #1 (SplitMix.chooseIntInf (lo, hi) g)

      fun suchThatOpt p g =
         let
            fun go (m, n) =
               if m > n
                  then pure NONE
               else
                  bind (resize (m, g))
                  (fn x => if p x then pure (SOME x) else go (m + 1, n))
         in
            sized (fn n => go (n, 2 * n))
         end

      fun suchThat p g =
         bind (suchThatOpt p g)
         (fn SOME x => pure x
           | NONE => sized (fn n => resize (n + 1, suchThat p g)))

      fun suchThatMap f g =
         map Option.valOf (suchThat Option.isSome (map f g))

      fun oneof [] = raise Fail "Gen.oneof: Empty list"
        | oneof [g] = g
        | oneof gs =
         bind (chooseInt (0, length gs - 1)) (fn n => List.nth (gs, n))

      fun elements [] = raise Fail "Gen.elements: Empty list"
        | elements [x] = pure x
        | elements xs =
         map (fn n => List.nth (xs, n)) (chooseInt (0, length xs - 1))

      fun frequency [] = raise Fail "Gen.frequency: Empty list"
        | frequency [(_, g)] = g
        | frequency xs =
         if List.exists (fn (n, _) => n < 0) xs
            then raise Fail "Gen.frequency: Negative weight"
         else if List.all (fn (n, _) => n = 0) xs
            then raise Fail "Gen.frequency: All weights are zero"
         else
            let
               val sum = List.foldr (fn ((n, _), acc) => n + acc) 0 xs
               fun pick ((k, g) :: xs) n =
                  if n <= k
                     then g
                  else pick xs (n - k)
                 | pick _ _ = raise Fail "Gen.frequency: Internal error"
            in
               bind (chooseInt (1, sum)) (pick xs)
            end

      fun listOfSize (0, g) = pure []
        | listOfSize (n, g) = bind g (fn x => map (fn xs => x :: xs) (listOfSize (n - 1, g)))

      fun listOf g = sized (fn n => bind (chooseInt (0, n)) (fn k => listOfSize (k, g)))

      fun int (g, _) = #1 (SplitMix.nextInt g)

      fun word (g, _) = #1 (SplitMix.nextWord g)

      fun coInt (n, g) = variant (IntInf.fromInt n, g)
      fun coWord (w, g) = variant (Word.toLargeInt w, g)
      fun coBool (true, g) = variant (0, g)
        | coBool (false, g) = variant (1, g)
      fun coOption _ (NONE, g) = variant (0, g)
        | coOption co (SOME x, g) = variant (1, co (x, g))

      fun generate (g, f) = f (g, 30)

      fun sample (g, gen) =
         let
            val sizes = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]
            val gens = List.map (fn n => resize (n, gen)) sizes
         in
            generate (g, sequence gens)
         end
   end

structure Shrink =
   struct
      type 'a t = 'a -> 'a Seq.t

      fun shrinkOption shr (SOME x) = Seq.cons (NONE, Seq.map SOME (shr x))
        | shrinkOption _ NONE = Seq.empty

      fun shrinkList shr xs =
         let
            val n = List.length xs

            fun shrinkOne [] = Seq.empty
              | shrinkOne (x :: xs) =
               Seq.append (Seq.map (fn x' => x' :: xs) (shr x),
                           Seq.map (fn xs' => x :: xs') (shrinkOne xs))

            fun removes (k, n, xs) =
               if k > n
                  then Seq.empty
               else
                  case List.splitAt (xs, n) of
                     (_, []) => Seq.singleton []
                   | (ys, zs) => Seq.cons (zs, Seq.map (fn zs' => ys @ zs') (removes (k, n - k, zs)))

            fun genRemoves k =
               if k > 0
                  then SOME (removes (k, n, xs), k div 2)
               else NONE
         in
            Seq.append (Seq.concat (Seq.unfold genRemoves n), shrinkOne xs)
         end

      local
         structure S = IntRedBlackSet

         fun nub xs =
            let
               fun go (s, seq) =
                  case Seq.uncons seq of
                     NONE => Seq.empty
                   | SOME (x, xs) =>
                        if S.member (s, x)
                           then go (s, xs)
                        else Seq.cons (x, go (S.add (s, x), xs))
            in
               go (S.empty, xs)
            end

         infix |<| quot

         val op quot = Int.quot

         fun a |<| b =
            case (a >= 0, b >= 0) of
               (true, true) => a < b
             | (false, false) => a > b
             | (true, false) => a + b < 0
             | (false, true) => a + b > 0

         fun go (n, q) =
            if q |<| n
               then Seq.cons (q, go (n, q quot 2))
            else Seq.empty
      in
         fun shrinkInt n =
            if n < 0 andalso ~n > n
               then nub (Seq.cons (~n, Seq.cons (0, go (n, n quot 2))))
            else nub (Seq.cons (0, go (n, n quot 2)))
      end

      fun shrinkReal x =
         if Real.isNan x
            then Seq.cons (0.0, Seq.unfold (fn n => if n < 1024.0 then SOME (n, n * 2.0) else NONE) 1.0)
         else if 2.0 * x + 1.0 <= x
            then Seq.unfold (fn n => if n < x then SOME (n, n * 2.0) else NONE) 1.0
         else
            (* TODO *)
            (* Seq.filter (fn y => Real.abs y < Real.abs x) *)
            Seq.empty

      fun shrinkNothing _ = Seq.empty
   end

structure Result =
   struct
      datatype t = T of {
         ok: bool option,
         expect: bool,
         reason: string,
         theExn: exn option,
         abort: bool,
         numTests: int option,
         checkCoverage: confidence option,
         labels: string list,
         classes: string list,
         tables: (string * string) list,
         testCase: string list
      }

      withtype confidence = {certainty: IntInf.int, tolerance: real}

      val stdConfidence = {certainty = IntInf.pow (10, 9), tolerance = 0.9}

      fun withOk (T r, ok) = T {
            ok = ok,
            expect = #expect r,
            reason = #reason r,
            theExn = #theExn r,
            abort = #abort r,
            numTests = #numTests r,
            checkCoverage = #checkCoverage r,
            labels = #labels r,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withExpect (T r, expect) = T {
            ok = #ok r,
            expect = expect,
            reason = #reason r,
            theExn = #theExn r,
            abort = #abort r,
            numTests = #numTests r,
            checkCoverage = #checkCoverage r,
            labels = #labels r,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withReason (T r, reason) = T {
            ok = #ok r,
            expect = #expect r,
            reason = reason,
            theExn = #theExn r,
            abort = #abort r,
            numTests = #numTests r,
            checkCoverage = #checkCoverage r,
            labels = #labels r,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withExn (T r, e) = T {
            ok = #ok r,
            expect = #expect r,
            reason = #reason r,
            theExn = e,
            abort = #abort r,
            numTests = #numTests r,
            checkCoverage = #checkCoverage r,
            labels = #labels r,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withAbort (T r, abort) = T {
            ok = #ok r,
            expect = #expect r,
            reason = #reason r,
            theExn = #theExn r,
            abort = abort,
            numTests = #numTests r,
            checkCoverage = #checkCoverage r,
            labels = #labels r,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withNumTests (T r, numTests) = T {
            ok = #ok r,
            expect = #expect r,
            reason = #reason r,
            theExn = #theExn r,
            abort = #abort r,
            numTests = numTests,
            checkCoverage = #checkCoverage r,
            labels = #labels r,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withCheckCoverage (T r, checkCoverage) = T {
            ok = #ok r,
            expect = #expect r,
            reason = #reason r,
            theExn = #theExn r,
            abort = #abort r,
            numTests = #numTests r,
            checkCoverage = checkCoverage,
            labels = #labels r,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withLabels (T r, labels) = T {
            ok = #ok r,
            expect = #expect r,
            reason = #reason r,
            theExn = #theExn r,
            abort = #abort r,
            numTests = #numTests r,
            checkCoverage = #checkCoverage r,
            labels = labels,
            classes = #classes r,
            tables = #tables r,
            testCase = #testCase r
         }

      fun withClasses (T r, classes) = T {
            ok = #ok r,
            expect = #expect r,
            reason = #reason r,
            theExn = #theExn r,
            abort = #abort r,
            numTests = #numTests r,
            checkCoverage = #checkCoverage r,
            labels = #labels r,
            classes = classes,
            tables = #tables r,
            testCase = #testCase r
         }

      val succeeded = T {
            ok = SOME true,
            expect = true,
            reason = "",
            theExn = NONE,
            abort = true,
            numTests = NONE,
            checkCoverage = NONE,
            labels = [],
            classes = [],
            tables = [],
            testCase = []
         }

      val failed = T {
            ok = SOME false,
            expect = true,
            reason = "",
            theExn = NONE,
            abort = true,
            numTests = NONE,
            checkCoverage = NONE,
            labels = [],
            classes = [],
            tables = [],
            testCase = []
         }

      val rejected = T {
            ok = NONE,
            expect = true,
            reason = "",
            theExn = NONE,
            abort = true,
            numTests = NONE,
            checkCoverage = NONE,
            labels = [],
            classes = [],
            tables = [],
            testCase = []
         }

      fun bool true = succeeded
        | bool false = T {
            ok = SOME false,
            expect = true,
            reason = "Falsified",
            theExn = NONE,
            abort = true,
            numTests = NONE,
            checkCoverage = NONE,
            labels = [],
            classes = [],
            tables = [],
            testCase = []
        }


   end

structure Property =
   struct
      type prop = Result.t Tree.t
      type t = prop Gen.t

      exception Discard

      (* fun exn (_, Discard) = discard *)
      (*   | exn (msg, e) = *) 
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
