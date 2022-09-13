
structure WordEx: WORD_EXTRA =
struct

open Word

val () =
   if wordSize <> 63
      then raise Fail "The CM interface assumes SML/NJ's 63-bit words"
   else ()

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

fun trailingZeros 0w0 = wordSize
  | trailingZeros w =
   let
      infix andb >>

      fun go (mask, set, w, n) =
         if (w andb mask) = 0w0
            then (w >> set, n + set)
         else (w, n)

      val (w, n) = go (0wxffffffff, 0w32, w, 0w0)
      val (w, n) = go (0wx0000ffff, 0w16, w, n)
      val (w, n) = go (0wx000000ff, 0w8, w, n)
      val (w, n) = go (0wx0000000f, 0w4, w, n)
      val (w, n) = go (0wx00000003, 0w2, w, n)
      val (w, n) = go (0wx00000001, 0w1, w, n)
   in
      toInt n
   end

(* This implementation is on-par with the SML/NJ library version *)
fun popCount 0wx7fffffffffffffff = 63
  | popCount w =
   let
      infix >> andb

      val w = w - ((w >> 0w1) andb 0wx5555555555555555)
      val w = (w andb 0wx3333333333333333) + ((w >> 0w2) andb 0wx3333333333333333)
      val w = ((w + (w >> 0w4)) andb 0wx0f0f0f0f0f0f0f0f) * 0wx0101010101010101
   in
      toInt (w >> 0w56)
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
