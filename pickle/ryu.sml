
structure Ryu :
sig

val toString : Real64.real -> string

end =
struct

(* Given 0≤a≤b≤c, returns (d, e) such that:
 * - a ≤ d ≤ c
 * - a < d*10^e < c
 * - e is maximal
 * - The resulting value for d is closest to b as possible
 * - The result respects the acceptSmaller and acceptLarger flags
 *)
fun computeShortest (a, b, c, acceptSmaller, acceptLarger) =
   let
      val digit = ref 0
      val i = ref 0
      val a = ref a
      val b = ref b
      val c = ref (if acceptLarger then c else c - 1)
      val allAZero = ref true
      val allBZero = ref true

      val () =
         while IntInf.quot (!a, 10) < IntInf.quot (!c, 10) do (
            allAZero := (!allAZero andalso IntInf.rem (!a, 10) = 0);
            a := IntInf.quot (!a, 10);
            c := IntInf.quot (!c, 10);
            allBZero := (!allBZero andalso !digit = 0);
            digit := IntInf.rem (!b, 10);
            b := IntInf.quot (!b, 10);
            i := !i + 1
         )
      val () =
         if acceptSmaller andalso !allAZero
            then
               while IntInf.rem (!a, 10) = 0 do (
                  a := IntInf.quot (!a, 10);
                  c := IntInf.quot (!c, 10);
                  digit := IntInf.rem (!b, 10);
                  allBZero := (!allBZero andalso !digit = 0);
                  b := IntInf.quot (!b, 10);
                  i := !i + 1)
         else ()

      val isTie = (!digit = 5) andalso !allBZero
      val breakTieDown = IntInf.andb (!b, 1) = 0
      val wantRoundDown = (!digit < 5) orelse (isTie andalso breakTieDown)
      val roundDown =
         (wantRoundDown andalso (!a <> !b orelse !allAZero))
         orelse
         (!b + 1 > !c)
   in
      if roundDown
         then (!b, !i)
      else (!b + 1, !i)
   end

(* Convert to base 10 mantissa and exponent *)
fun toManExp (r : Real64.real) : {sign: bool, mantissa: IntInf.int, exponent: int} =
   let
      (* Step 1 - Decode the number *)
      (* Convert the floating point number into its binary representation *)
      val bytes = PackReal64Little.toBytes r
      val w = PackWord64Little.subVec (bytes, 0)

      (* Extract the binary components *)
      val sign = LargeWord.andb (w, 0wx8000000000000000) <> 0w0
      val ieeeExponent = LargeWord.andb (LargeWord.>> (w, 0w52), 0wx7FF)
      val ieeeMantissa = LargeWord.andb (w, 0wxFFFFFFFFFFFFF)

      (* Check for NaN, ±Infinity, 0 *)
      val () =
         if ieeeExponent = 0wx7FF
            then raise Fail "toManExp: NaN or Infinity"
         else if ieeeExponent = 0w0 andalso ieeeMantissa = 0w0
            then raise Fail "toManExp: zero"
         else ()

      (* Normalize the exponent and mantissa, handling subnormals *)
      val (mf, ef) =
         if ieeeExponent = 0w0
            then (ieeeMantissa, ~1074)
         else
            (LargeWord.orb (0wx10000000000000, ieeeMantissa),
             Int32.fromLarge (LargeWord.toLargeInt ieeeExponent) - 1075)

      (* Step 2 - Determine the interval of information-preserving outputs *)
      val e2 = ef - 2
      val u = 0w4 * mf - (if ieeeMantissa = 0w0 andalso ieeeExponent > 0w1 then 0w1 else 0w2)
      val v = 0w4 * mf
      val w = 0w4 * mf + 0w2

      (* Step 3 - convert (u, v, w)×2^e₂ to a decimal power base *)
      (* (a, b, c)×10^e₁₀ == (u, v, w)×2^e₂ *)
      val (e10, a, b, c) =
         if e2 >= 0
            then
               let
                  val x = Word.fromLargeInt (Int32.toLarge e2)
                  fun f y = IntInf.<< (LargeWord.toLargeInt y, x)
               in
                  (0, f u, f v, f w)
               end
         else
            let
               val x = IntInf.pow (5, Int32.toInt (~e2))
               fun f y = LargeWord.toLargeInt y * x
            in
               (e2, f u, f v, f w)
            end

      (* Step 4 - find the shortest, correctly rounded decimal representation *)
      val even = LargeWord.andb (ieeeMantissa, 0w1) = 0w0
      val acceptSmaller = even
      val acceptLarger = even
      val (d0, e0) = computeShortest (a, b, c, acceptSmaller, acceptLarger)
   in
      {sign=sign, mantissa=d0, exponent=e0 + Int32.toInt e10}
   end

fun decimalLength (v : IntInf.int) : int =
   if v >= 1000000000000000000 then 19 else
   if v >= 100000000000000000 then 18 else
   if v >= 10000000000000000 then 17 else
   if v >= 1000000000000000 then 16 else
   if v >= 100000000000000 then 15 else
   if v >= 10000000000000 then 14 else
   if v >= 1000000000000 then 13 else
   if v >= 100000000000 then 12 else
   if v >= 10000000000 then 11 else
   if v >= 1000000000 then 10 else
   if v >= 100000000 then 9 else
   if v >= 10000000 then 8 else
   if v >= 1000000 then 7 else
   if v >= 100000 then 6 else
   if v >= 10000 then 5 else
   if v >= 1000 then 4 else
   if v >= 100 then 3 else
   if v >= 10 then 2 else
   1

fun toString r =
   if Real64.== (r, 0.0)
      then "0"
   else if Real64.== (r, Real64.posInf)
      then "Infinity"
   else if Real64.== (r, Real64.negInf)
      then "-Infinity"
   else
      let
         val {sign, mantissa, exponent} = toManExp r
         val sign = if sign then "-" else ""
         val len = decimalLength mantissa
         val k = exponent
         val kk = len + k
         fun pad n = CharVector.tabulate (n, fn _ => #"0")
      in
         if 0 <= k andalso kk <= 21
            then
               concat [sign, IntInf.toString mantissa, pad (kk - len)]
         else if 0 < kk andalso kk <= 21
            then
               concat
               [sign,
                String.substring (IntInf.toString mantissa, 0, kk),
                ".",
                String.extract (IntInf.toString mantissa, kk, NONE)]
         else if ~6 < kk andalso kk <= 0
            then
               concat [sign, "0.", pad (~kk), IntInf.toString mantissa]
         else if len = 1
            then
               concat
               [sign,
                IntInf.toString mantissa,
                "e",
                if kk-1 < 0 then "-" else "+",
                Int.toString (Int.abs (kk-1))]
         else
            concat
            [sign,
             String.substring (IntInf.toString mantissa, 0, 1),
             ".",
             String.extract (IntInf.toString mantissa, 1, NONE),
             "e",
             if kk-1 < 0 then "-" else "+",
             Int.toString (Int.abs (kk-1))]
      end

end

(* vim: set ts=3 sw=3: *)
