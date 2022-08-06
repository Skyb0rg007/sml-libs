
structure SplitMix: SPLITMIX =
struct

infix << >> andb orb xorb
val op << = Word64.<<
val op >> = Word64.>>
val op andb = Word64.andb
val op orb = Word64.orb
val op xorb = Word64.xorb

(* Internal *)

val goldenGamma: Word64.word = 0wx9e3779b97f4a7c15

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

fun popCount w =
   let
      val w = w - ((w >> 0w1) andb 0wx5555555555555555)
      val w = (w andb 0wx3333333333333333) + ((w >> 0w2) andb 0wx3333333333333333)
      val w = (w + (w >> 0w4)) andb 0wx0f0f0f0f0f0f0f0f
   in
      Word64.toInt ((w * 0wx0101010101010101) >> 0w56)
   end

fun leadingZeros 0w0 = 0w64
  | leadingZeros w =
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

fun mixGamma w =
   let
      val w = mix64 w orb 0w1
   in
      if popCount (w xorb (w >> 0w1)) < 24
         then w xorb 0wxaaaaaaaaaaaaaaaa
      else w
   end

(* Core API *)

datatype t = T of {seed: Word64.word, gamma: Word64.word}

fun toString (T {seed, gamma}) =
   String.concat
   [ "SplitMix {seed = 0wx", Word64.fmt StringCvt.HEX seed,
     ", gamma = 0wx", Word64.fmt StringCvt.HEX gamma, "}"]

fun fromSeed seed =
   T {seed = mix64 seed, gamma = mixGamma (seed + goldenGamma)}

fun split (T {seed, gamma}) =
   let
      val seed' = seed + gamma
      val seed'' = seed' + gamma
   in
      (T {seed = seed'', gamma = gamma},
       T {seed = mix64variant13 seed', gamma = mixGamma seed''})
   end

fun nextWord64 (T {seed, gamma}) =
   let
      val seed' = seed + gamma
   in
      (mix64variant13 seed', T {seed = seed', gamma = gamma})
   end

(* Additional operations *)

val realUlp = 1.0 / Real.fromLargeInt (Word64.toLargeInt (0w1 << 0w24))

fun nextReal g =
   let
      val (w, g') = nextWord64 g
      val r = Real.fromLargeInt (Word64.toLargeInt (w >> 0w11))
   in
      (r * realUlp, g')
   end

fun chooseWord64Upto range =
   let
      val mask = Word64.notb 0w0 >> leadingZeros range

      fun go g =
         let
            val (x, g') = nextWord64 g
            val x' = x andb mask
         in
            if x' > range
               then go g'
            else (x', g')
         end
   in
      go
   end

fun chooseWord64 (lo, hi) g =
   case Word64.compare (lo, hi) of
      LESS => let val (w, g') = chooseWord64Upto (hi - lo) g in (w + lo, g') end
    | EQUAL => (lo, g)
    | GREATER => let val (w, g') = chooseWord64Upto (lo - hi) g in (w + hi, g') end

fun chooseInt (lo, hi) g =
   case Int.compare (lo, hi) of
      LESS =>
         let
            val (w, g') = chooseWord64Upto (Word64.fromInt hi - Word64.fromInt lo) g
         in
            (Word64.toInt (w + Word64.fromInt lo), g')
         end
    | EQUAL => (lo, g)
    | GREATER =>
         let
            val (w, g') = chooseWord64Upto (Word64.fromInt lo - Word64.fromInt hi) g
         in
            (Word64.toInt (w + Word64.fromInt hi), g')
         end

fun chooseWord (lo, hi) g =
   case Word.compare (lo, hi) of
      LESS =>
         let
            val (w, g') = chooseWord64Upto (Word.toLarge hi - Word.toLarge lo) g
         in
            (Word.fromLarge (w + Word.toLarge lo), g')
         end
    | EQUAL => (lo, g)
    | GREATER =>
         let
            val (w, g') = chooseWord64Upto (Word.toLarge lo - Word.toLarge hi) g
         in
            (Word.fromLarge (w + Word.toLarge hi), g')
         end


local
   val prec = Option.getOpt (Int.precision, 64)
   val signMask = 0w1 << 0w64
   val numMask = (0w1 << Word.fromInt (prec - 1)) - 0w1
   val mask = signMask orb numMask
in
   fun nextInt g =
      let
         val (w, g') = nextWord64 g
      in
         (Word64.toIntX (w andb mask), g')
      end
end

local
   val mask = (0w1 << Word.fromInt Word.wordSize) - 0w1
in
   fun nextWord g =
      let
         val (w, g') = nextWord64 g
      in
         (Word.fromLarge (w andb mask), g')
      end
end

val two64 = IntInf.pow (2, 64)

fun chooseIntInfUpto range =
   let
      val (leadMask, restDigits) =
         let
            fun go (n, x) =
               if x < two64
                  then (Word64.notb 0w0 >> leadingZeros (Word64.fromLargeInt x), n)
               else go (n + 0w1, IntInf.~>> (x, 0w64))
         in
            go (0w0, range)
         end

      fun generate g0 =
         let
            fun go (acc, 0w0, g) = (acc, g)
              | go (acc, n, g) =
               let
                  val (x, g') = nextWord64 g
               in
                  go (acc * two64 + Word64.toLargeInt x, n - 0w1, g')
               end

            val (x, g') = nextWord64 g0
            val x' = x andb leadMask
         in
            go (Word64.toLargeInt x', restDigits, g')
         end

      fun loop g =
         let
            val (x, g') = generate g
         in
            if x > range
               then loop g'
            else (x, g')
         end
   in
      loop
   end

fun chooseIntInf (lo, hi) g =
   case IntInf.compare (lo, hi) of
      LESS => let val (i, g') = chooseIntInfUpto (hi - lo) g in (i + lo, g') end
    | EQUAL => (lo, g)
    | GREATER => let val (i, g') = chooseIntInfUpto (lo - hi) g in (i + hi, g') end

fun new () =
   let
      val now = Time.toMilliseconds (Time.now ())
   in
      fromSeed (Word64.fromLargeInt now)
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
