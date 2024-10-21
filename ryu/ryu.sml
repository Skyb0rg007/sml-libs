
structure Ryu =
struct

structure R64 = Real64
structure W64 = Word64
structure W32 = Word32
structure W = Word
structure I = Int

val () =
   case I.precision of
      NONE => ()
    | SOME n => if n >= 32 then () else raise Fail "Int must be >= 32 bits"

(* Computes ⌊log_10(2^e)⌋ *)
fun log10Pow2 (e: int): int =
   if 0 <= e andalso e <= 1650
      then W32.toInt (W32.>> (W32.fromInt e * 0w78913, 0w18))
   else raise Domain

(* Computes ⌈log_2(5^e)⌉. Returns 1 if e = 0. *)
fun pow5bits (e: int): int =
   if 0 <= e andalso e <= 3528
      then W32.toInt (W32.>> (W32.fromInt e * 0w1217359, 0w19) + 0w1)
   else raise Domain

(* Computes `(hi, lo) >> w` on the 128-bit word (hi, lo) *)
fun shiftRight128 (lo: W64.word, hi: W64.word, dist: word): W64.word =
   if 0w0 < dist andalso dist < 0w64
      then W64.orb (W64.<< (hi, 0w64 - dist), W64.>> (lo, dist))
   else raise Domain

fun umul128 (a, b) =
   let
      val aLo = W64.andb (a, 0wxFFFFFFFF)
      val aHi = W64.andb (W64.>> (a, 0w32), 0wxFFFFFFFF)
      val bLo = W64.andb (b, 0wxFFFFFFFF)
      val bHi = W64.andb (W64.>> (b, 0w32), 0wxFFFFFFFF)

      val b00 = aLo * bLo
      val b01 = aLo * bHi
      val b10 = aHi * bLo
      val b11 = aHi * bHi

      val b00Lo = W64.andb (b00, 0wxFFFFFFFF)
      val b00Hi = W64.andb (W64.>> (b00, 0w32), 0wxFFFFFFFF)

      val mid1 = b10 + b00Hi
      val mid1Lo = W64.andb (mid1, 0wxFFFFFFFF)
      val mid1Hi = W64.andb (W64.>> (mid1, 0w32), 0wxFFFFFFFF)

      val mid2 = b01 + mid1Lo
      val mid2Lo = W64.andb (mid2, 0wxFFFFFFFF)
      val mid2Hi = W64.andb (W64.>> (mid2, 0w32), 0wxFFFFFFFF)

      val pHi = b11 + mid1Hi + mid2Hi
      val pLo = W64.orb (W64.<< (mid2Lo, 0w32), b00Lo)
   in
      (pHi, pLo)
   end

fun mulShiftAll64 (m, mulhi, mullo, j, mmShift) =
   let
      val m = W64.<< (m, 0w1)
      val (tmp, lo) = umul128 (m, mullo)
      val (hi, mid) = umul128 (m, mulhi)
      val hi = if mid < tmp then hi + 0w1 else hi

      val lo2 = lo + mullo
      val mid2 = mid + mulhi + (if lo2 < lo then 0w1 else 0w0)
      val hi2 = hi + (if mid2 < mid then 0w1 else 0w0)
      val vp = shiftRight128 (mid2, hi2, j - 0w64 - 0w1)

   in
      ()
   end

fun d2d (ieeeMantissa: W64.word, ieeeExponent: int) =
   let
      (* Step 1: Interpret the encoded data *)
      (* This handles normals, subnormals, and their stored biases *)
      (* Number is represented as `value = m2 * 2^(e2+2)`
       * (the bias on e2 is to simplify future calculations) *)
      val (e2: int, m2: W64.word) =
         if ieeeExponent = 0
            then (1 - 1023 - 52 - 2, ieeeMantissa)
         else (ieeeExponent - 1023 - 52 - 2, W64.orb (ieeeMantissa, 0wx10000000000000))
      val even: bool = W64.andb (m2, 0w1) = 0w0
      (* val acceptBounds = even *)

      val () =
         print ("-> " ^ W64.fmt StringCvt.DEC m2 ^ " * 2^" ^ Int.toString (e2 + 2) ^ "\n")

      (* Step 2: Determine the interval of valid decimal representations *)
      val mv: W64.word = W64.<< (m2, 0w2)
      val mmShift = ieeeMantissa <> 0w0 orelse ieeeExponent <= 1
      (* val mp: W64.word = mv + 0w2 *)
      (* val mm: W64.word = mv - (if mmShift then 0w2 else 0w1) *)

      (* Step 3: Convert to a decimal power base using 128-bit arithmetic *)
      val {vr, vp, vm, e10, vmIsTrailingZeros, vrIsTrailingZeros} =
         if e2 >= 0
            then
               let
                  (* q = max(0, floor(log10(2^e2)) - 1) *)
                  val q = log10Pow2 e2 - (if e2 > 3 then 1 else 0)
                  (* k = B0 + floor(log2(5^q)) *)
                  val k = 125 - pow5bits q - 1
                  val i = ~e2 + q + k
                  (* val pow5 = computeInvPow5 q *)
                  (* val (vr, vp, vm) = mulShiftAll64 (m2, pow5, i, mmShift) *)
                  val e10 = q
               in
                  raise Fail ""
               end
         else raise Fail ""
   in
      raise Fail ""
   end

fun multiplePowOf5 (_, 0) = true
  | multiplePowOf5 (q, 1) = Int.rem (q, 5) = 0
  | multiplePowOf5 (q, 2) = Int.rem (q, 25) = 0
  | multiplePowOf5 (q, 3) = Int.rem (q, 125) = 0
  | multiplePowOf5 (q, 4) = Int.rem (q, 625) = 0
  | multiplePowOf5 (q, 5) = Int.rem (q, 3125) = 0
  | multiplePowOf5 (q, 6) = Int.rem (q, 15625) = 0
  | multiplePowOf5 (q, n) =
   let
      val q' = Int.quot (q, 78125)
   in
      q' * 78125 = q andalso multiplePowOf5 (q', n - 7)
   end

fun multiplePowOf5' (q, n) =
   Int.rem (q, Int.fromLarge (IntInf.pow (5, n))) = 0

fun mulPow5DivPow2 (m, s0l, s0h, s1l, s1h, j) =
   let
      infix >> <<
      val op >> = W64.>>
      val op << = W64.<<
      val ml = W64.andb (m, 0wx7FFFFFFF)
      val mh = W64.>> (m, 0w31)
   in
      (((((ml * s0l >> 0w31) + ml * s0h + mh * s0l >> 0w31) + ml * s1l + mh * s0h >> 0w31) + ml * s1h + mh * s1l >> 0w21) + (mh * s1h << 0w10)) >> j
   end

val MANTISSA_BITS : word = 0w52
val EXPONENT_BITS : word = 0w11
val MANTISSA_MASK : W64.word = W64.<< (0w1, MANTISSA_BITS) - 0w1
val EXPONENT_MASK : W64.word = W64.<< (0w1, EXPONENT_BITS) - 0w1
val EXPONENT_BIAS : int = W64.toInt (W64.<< (0w1, EXPONENT_BITS - 0w1) - 0w1)

val LOG10_2_DENOMINATOR = 10000000
val LOG10_2_NUMERATOR = R64.toInt IEEEReal.TO_NEAREST (R64.fromInt LOG10_2_DENOMINATOR * R64.Math.log10 2.0)

fun toBits (value: R64.real): W64.word =
   Unsafe.realToBits value

fun decodeReal (value: R64.real): {sign: bool, exp: int, man: W64.word} =
   let
      val bits = toBits value
   in
      {sign = W64.andb (bits, 0wx8000000000000000) <> 0w0,
       exp = W64.toInt (W64.andb (W64.>> (bits, MANTISSA_BITS), EXPONENT_MASK)),
       man = W64.andb (bits, MANTISSA_MASK)}
   end

fun toStringNormalSubnormal (value: R64.real): string =
   let
      (* Step 1: Decode the number, and unify normal and subnormal cases *)
      (* Extract the mantissa and exponent bits *)
      val {sign, exp = ieeeExponent, man = ieeeMantissa} = decodeReal value
      (* Determine the mantissa and exponent that these bits represent *)
      (* e2 - binary exponent *)
      (* m2 - binary mantissa *)
      val (e2: int, m2: W64.word) =
         if ieeeExponent = 0
            then
               (* Subnormal *)
               (~1074, ieeeMantissa)
         else
            (* Normal *)
            (ieeeExponent - 1075, W64.orb (ieeeMantissa, 0wx10000000000000))
      (* Step 2: Determine the interval of legal decimal representations *)
      val even: bool = W64.andb (m2, 0w1) = 0w0
      val mv: W64.word = W64.<< (m2, 0w2)
      val mp: W64.word = mv + 0w2
      val mmShift =
         if ieeeMantissa = 0w0 andalso ieeeExponent > 1
            then 0w1
         else 0w2
      val mm: W64.word = mv - mmShift
      val e2: int = e2 - 2

      (* val () = *)
      (*    if e2 >= 0 *)
      (*       then ( *)
      (*          print ("E = 0\n"); *)
      (*          print ("d+=" ^ IntInf.toString (IntInf.<< (W64.toLargeInt mp, Word.fromInt e2)) ^ "\n"); *)
      (*          print ("d =" ^ IntInf.toString (IntInf.<< (W64.toLargeInt mv, Word.fromInt e2)) ^ "\n"); *)
      (*          print ("d-=" ^ IntInf.toString (IntInf.<< (W64.toLargeInt mm, Word.fromInt e2)) ^ "\n"); *)
      (*          print ("e2=" ^ Int.toString e2 ^ "\n")) *)
      (*    else *)
      (*       let val factor = IntInf.pow (5, ~e2) in *)
      (*          print ("E =" ^ IntInf.toString factor ^ "\n"); *)
      (*          print ("d+=" ^ IntInf.toString (W64.toLargeInt mp * factor) ^ "\n"); *)
      (*          print ("d =" ^ IntInf.toString (W64.toLargeInt mv * factor) ^ "\n"); *)
      (*          print ("d-=" ^ IntInf.toString (W64.toLargeInt mm * factor) ^ "\n"); *)
      (*          print ("e2=" ^ Int.toString e2 ^ "\n") *)
      (*       end *)

      val {dv, dp, dm, e10, dpIsTrailingZeros, dmIsTrailingZeros} =
         if e2 >= 0
            then
               let
                  val q = Int.max (0, W64.toInt (W64.>> (W64.fromInt e2 * 0w1292913986, 0w32)) - 1)
                  val j = q * 9972605231
               in
                  {dv = 0w0,
                   dp = 0w0,
                   dm = 0w0,
                   e10 = 0w0,
                   dpIsTrailingZeros = false,
                   dmIsTrailingZeros = false}
               end
         else raise Fail ""
   in
      ""
   end

fun toString value =
   if R64.isNan value
      then "NaN"
   else if R64.== (value, R64.posInf)
      then "Infinity"
   else if R64.== (value, R64.negInf)
      then "-Infinity"
   else if R64.== (value, 0.0)
      then "0.0"
   else toStringNormalSubnormal value

(* fun real64ToBits r = *)
(*    let *)
(*       val b = PackR64Little.toBytes r *)
(*    in *)
(*       PackW64Little.subVec (b, 0) *)
(*    end *)



(* digits *)
(* sign (bits) *)
(* decimalNotation *)
(* exp *)
(* dv *)


end

(* vim: set ts=3 sw=3:*)
