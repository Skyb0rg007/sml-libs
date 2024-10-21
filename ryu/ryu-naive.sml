
structure Ryu' =
struct

structure R64 = Real64
structure W64 = Word64

val MANTISSA_BITS : word = 0w52
val EXPONENT_BITS : word = 0w11
val MANTISSA_MASK : W64.word = W64.<< (0w1, MANTISSA_BITS) - 0w1
val EXPONENT_MASK : W64.word = W64.<< (0w1, EXPONENT_BITS) - 0w1
val EXPONENT_BIAS : int = W64.toInt (W64.<< (0w1, EXPONENT_BITS - 0w1) - 0w1)

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

fun toStringNormalSubnormal {sign, exp=ieeeExponent, man=ieeeMantissa} =
   let
      (* Step 1: Decode the number, and unify normal and subnormal cases *)
      (* ef - binary exponent *)
      (* mf - binary mantissa *)
      val (ef: int, mf: W64.word) =
         if ieeeExponent = 0
            then
               (* Subnormal *)
               (~1074, ieeeMantissa)
         else
            (* Normal *)
            (ieeeExponent - 1075, W64.orb (ieeeMantissa, 0wx10000000000000))
      val even = W64.andb (mf, 0w1) = 0w0
      (* Step 2: Determine the interval of legal decimal representations *)
      val e2 = ef - 2
      val u = 0w4 * mf - (if ieeeMantissa = 0w0 andalso ieeeExponent > 1 then 0w1 else 0w2)
      val v = 0w4 * mf
      val w = 0w4 * mf + 0w2
      val (a, b, c, e10) =
         if e2 >= 0
            then
               let
                  val q = IntInf.pow (2, e2)
                  fun mul x = W64.toLargeInt x * q
               in
                  (mul u, mul v, mul w, 0)
               end
         else
            let
               val q = IntInf.pow (5, ~e2)
               fun mul x = W64.toLargeInt x * q
            in
               (mul u, mul v, mul w, 10)
            end
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
   else toStringNormalSubnormal (decodeReal value)


end

(* vim: set ts=3 sw=3:*)
