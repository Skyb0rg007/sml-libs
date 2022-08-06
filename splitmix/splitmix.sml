
structure SplitMix =
struct
   infix << >> andb orb xorb
   val op << = Word64.<<
   val op >> = Word64.>>
   val op andb = Word64.andb
   val op orb = Word64.orb
   val op xorb = Word64.xorb

   val GOLDEN_GAMMA: Word64.word = 0wx9e3779b97f4a7c15
   val REAL_ULP: real = 1.0 / Math.pow (2.0, Real.fromInt Real.precision)

   val w64ToReal = Real.fromLargeInt o Word64.toLargeInt
   val w64ToInf = IntInf.fromLarge o Word64.toLargeInt
   val w64ToI64 = Int64.fromLarge o Word64.toLargeIntX
   val infToW64 = Word64.fromLargeInt o IntInf.toLarge

   fun popCount w =
      let
         val w = w - ((w >> 0w1) andb 0wx5555555555555555)
         val w = (w andb 0wx3333333333333333) + ((w >> 0w2) andb 0wx3333333333333333)
         val w = (w + (w >> 0w4)) andb 0wx0f0f0f0f0f0f0f0f
      in
         Word64.toInt ((w * 0wx0101010101010101) >> 0w56)
      end

   fun mix64 w =
      let
         val w = (w xorb (w >> 0w33)) * 0wxff51afd7ed558ccd
         val w = (w xorb (w >> 0w33)) * 0wxc4ceb9fe1a85ec53
      in
         w xorb (w >> 0w33)
      end

   fun mix64variant13 w =
      let
         val w = (w xorb (w >> 0w30)) * 0wxbf58476d1ce4e5b9
         val w = (w xorb (w >> 0w27)) * 0wx94d049bb133111eb
      in
         w xorb (w >> 0w31)
      end

   fun mixGamma w =
      let
         val w = mix64variant13 w orb 0w1
         val n = popCount (w xorb (w >> 0w1))
      in
         if n >= 24
            then w
         else w xorb 0wxaaaaaaaaaaaaaaaa
      end

   fun clz64 0w0 = 0w64
     | clz64 w =
      let
         fun go (mask, size, w, n) =
            if (w andb mask) = 0w0
               then (w << size, n + size) 
            else (w, n)

         val (w, n) = go (0wxffffffff00000000, 0w32, w, 0w0)
         val (w, n) = go (0wxffff000000000000, 0w16, w, n)
         val (w, n) = go (0wxff00000000000000, 0w8, w, n)
         val (w, n) = go (0wxf000000000000000, 0w4, w, n)
         val (w, n) = go (0wxc000000000000000, 0w2, w, n)
         val (_, n) = go (0wx8000000000000000, 0w1, w, n)
      in
         n
      end

   fun initialSeed () =
      Word64.notb (Word64.fromLargeInt (Time.toSeconds (Time.now ())))

   datatype t = T of { seed: Word64.word, gamma: Word64.word }

   fun fromSeed w = T { seed = mix64 w, gamma = mixGamma (w + GOLDEN_GAMMA) }

   fun split (T {seed, gamma}) =
      let
         val seed' = seed + gamma
         val seed'' = seed' + gamma
      in
         (T {seed = seed'', gamma = gamma},
          T {seed = mix64 seed', gamma = mixGamma seed''})
      end

   fun nextWord64 (T {seed, gamma}) =
      let
         val seed' = seed + gamma
      in
         (mix64 seed', T {seed = seed', gamma = gamma})
      end

   fun nextInt64 g =
      case nextWord64 g of
         (w, g') => (w64ToI64 w, g')

   fun nextReal g =
      case nextWord64 g of
         (w, g') => (w64ToReal (w >> 0w11) * REAL_ULP, g')

   local
      val two64 = IntInf.pow (2, 64)

      fun nextIntInf' (range: IntInf.int, g: t): IntInf.int * t =
         let
            val (leadMask: Word64.word, restDigits: Word.word) =
               let
                  fun go (n, x) =
                     if x < two64
                        then (Word64.notb 0w0 >> clz64 (infToW64 x), n)
                     else go (n + 0w1, IntInf.~>> (x, 0w64))
               in
                  go (0w0, range)
               end

            fun generate (g: t): IntInf.int * t =
               let
                  fun go (acc, 0w0, g) = (acc, g)
                    | go (acc, n, g) =
                        let
                           val (x, g') = nextWord64 g
                        in
                           go (acc * two64 + w64ToInf x, n - 0w1, g')
                        end
                  val (x, g') = nextWord64 g
               in
                  go (w64ToInf (x andb leadMask), restDigits, g')
               end

            fun loop (g: t): IntInf.int * t =
               let
                  val (x, g') = generate g
               in
                  if x > range
                     then loop g'
                  else (x, g')
               end
         in
            loop g
         end
   in
      fun nextIntInf (lo, hi, g) =
         case IntInf.compare (lo, hi) of
            EQUAL => (lo, g)
          | LESS =>
               let
                  val (i, g') = nextIntInf' (hi - lo, g)
               in
                  (lo + i, g')
               end
          | GREATER =>
               let
                  val (i, g') = nextIntInf' (lo - hi, g)
               in
                  (hi + i, g')
               end
   end

   val nextInt =
      case Int.precision of
         NONE =>
            (fn g =>
               let
                  val min = ~(IntInf.pow (2, 63))
                  val max = IntInf.pow (2, 63) - 1
                  val (i, g') = nextIntInf (min, max, g)
               in
                  (Int.fromLarge (IntInf.toLarge i), g')
               end)
       | SOME p =>
            if p <= 64
               then fn g =>
                  let
                     val (w, g') = nextWord64 g
                     val mag = w andb ((0w1 << Word.fromInt (p - 1)) - 0w1)
                     val sign = (w andb (0w1 << Word.fromInt p)) <> 0w0
                     val neg = if sign then Int.~ else Fn.id
                  in
                     (neg (Word64.toIntX mag), g')
                  end
            else
               fn g =>
                  let
                     val min = ~(IntInf.pow (2, p - 1))
                     val max = IntInf.pow (2, p - 1) - 1
                     val (i, g') = nextIntInf (min, max, g)
                  in
                     (Int.fromLarge (IntInf.toLarge i), g')
                  end


end

(* vim: set tw=0 ts=3 sw=3: *)
