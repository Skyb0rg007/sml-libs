
structure SExpNum: SEXP_NUMERIC_TOWER =
struct

datatype t =
   FIX of int
 | BIG of IntInf.int
 | RAT of t * t
 | FLO of real
 | CPX of t * t

val inIntRange: IntInf.int -> bool =
   case (Int.minInt, Int.maxInt) of
      (SOME min, SOME max) =>
         let
            val min = IntInf.fromInt min
            val max = IntInf.fromInt max
         in
            fn n => min <= n andalso n <= max
         end
    | (NONE, NONE) => Fn.const true
    | _ => raise Fail "Invalid Int.int bounds"

val (minInt, maxInt): real * real =
   case (Int.minInt, Int.maxInt) of
      (SOME min, SOME max) => (Real.fromInt min, Real.fromInt max)
    | (NONE, NONE) => (Real.negInf, Real.posInf)
    | _ => raise Fail "Invalid Int.int bounds"

val fix = FIX

fun big n =
   if inIntRange n
      then FIX (IntInf.toInt n)
   else BIG n

val flo = FLO

fun cpx (CPX _, _) = raise Fail "Complex number with cpxnum for real part"
  | cpx (_, CPX _) = raise Fail "Complex number with cpxnum for imag part"
  | cpx (a, FIX 0) = a
  | cpx (a, b) = CPX (a, b)

local
   fun gcdInt (0, b) = b
     | gcdInt (a, 0) = a
     | gcdInt (a, b) = gcdInt (Int.mod (b, a), a)

   fun gcdInf (0, b) = b
     | gcdInf (a, 0) = a
     | gcdInf (a, b) = gcdInf (IntInf.mod (b, a), a)

   fun toInf (FIX n) = SOME (IntInf.fromInt n)
     | toInf (BIG n) = SOME n
     | toInf _ = NONE
in
   fun rat (a as FIX _, FIX 1) = a
     | rat (a as BIG _, FIX 1) = a
     | rat (FIX 0, FIX _) = FIX 0
     | rat (FIX 0, BIG _) = FIX 0
     | rat (FIX n, FIX d) =
      let
         val gcd = gcdInt (n, d)
      in
         if d = gcd
            then fix (n div gcd)
         else RAT (fix (n div gcd), fix (d div gcd))
      end
     | rat (n, d) =
      case (toInf n, toInf d) of
         (SOME n, SOME d) =>
            let
               val gcd = gcdInf (n, d)
            in
               if d = gcd
                  then big (n div gcd)
               else RAT (big (n div gcd), big (d div gcd))
            end
       | (NONE, _) => raise Fail "Rational number with inexact numerator"
       | (_, NONE) => raise Fail "Rational number with inexact denominator"

   fun valid (FIX _) = true
     | valid (BIG n) = not (inIntRange n)
     | valid (RAT (FIX 0, _)) = false
     | valid (RAT (_, FIX 1)) = false
     | valid (RAT (n, d)) =
      let
         fun toBig (FIX n) = SOME (IntInf.fromInt n)
           | toBig (BIG n) = if inIntRange n then NONE else SOME n
           | toBig _ = NONE
      in
         case (toBig n, toBig d) of
            (SOME a, SOME b) => gcdInf (a, b) = b
          | _ => false
      end
     | valid (FLO _) = true
     | valid (CPX (_, FIX 0)) = false
     | valid (CPX (a, b)) =
      let
         fun notCpx (CPX _) = false
           | notCpx _ = true
      in
         notCpx a andalso notCpx b andalso valid a andalso valid b
      end
end

fun toString (FIX n) =
   String.map (fn #"~" => #"-" | c => c) (Int.toString n)
  | toString (BIG n) =
   String.map (fn #"~" => #"-" | c => c) (IntInf.toString n)
  | toString (RAT (n, d)) =
   String.concat [toString n, "/", toString d]
  | toString (FLO n) =
   String.map (fn #"~" => #"-" | #"E" => #"e" | c => c) (Real.toString n)
  | toString (CPX (a, b)) =
   let
      val astr = toString a
      val bstr = toString b
   in
      if String.sub (bstr, 0) = #"-"
         then String.concat [astr, bstr, "i"]
      else String.concat [astr, "+", bstr, "i"]
   end

fun same (FIX a, FIX b) = a = b
  | same (BIG a, BIG b) = a = b
  | same (FLO a, FLO b) = Real.== (a, b) orelse (Real.isNan a andalso Real.isNan b)
  | same (RAT (n, d), RAT (n', d')) = same (n, n') andalso same (d, d')
  | same (CPX (a, b), CPX (a', b')) = same (a, a') andalso same (b, b')
  | same _ = false

fun quotient (CPX _, _) = raise Fail "Cannot take quotient of complex number"
  | quotient (_, CPX _) = raise Fail "Cannot take quotient of complex number"
  | quotient (FIX a, FIX b) = fix (a div b)
  | quotient (FIX _, BIG _) = fix 0
  | quotient (BIG a, BIG b) = big (a div b)
  | quotient (BIG a, FIX b) = big (a div IntInf.fromInt b)
  | quotient _ = raise Fail "NYI"
   

fun exactIntCompare (FIX a, FIX b) = Int.compare (a, b)
  | exactIntCompare (BIG a, BIG b) = IntInf.compare (a, b)
  | exactIntCompare (FIX a, BIG b) = IntInf.compare (IntInf.fromInt a, b)
  | exactIntCompare (BIG a, FIX b) = IntInf.compare (a, IntInf.fromInt b)
  | exactIntCompare _ = raise Fail "exactIntCompare given non-exact-integer"

local
   fun ratioToReal (n, d) =
      let
         fun toReal (FIX n) = Real.fromInt n
           | toReal (BIG n) = Real.fromLargeInt n
           | toReal _ = raise Fail "ratioToReal: non-exact-integer"

         fun quotient (FIX a, FIX b) = Real.fromInt (a div b)
           | quotient (BIG a, BIG b) = Real.fromLargeInt (a div b)
           | quotient (FIX a, BIG b) = Real.fromLargeInt (IntInf.fromInt a div b)
           | quotient (BIG a, FIX b) = Real.fromLargeInt (a div IntInf.fromInt b)
           | quotient _ = raise Fail "ratioToReal: non-exact-integer"

         val res = toReal n / toReal d
      in
         if Real.isFinite res
            then res
         else
            case exactIntCompare (n, d) of
               LESS => 1.0 / quotient (d, n)
             | _ => quotient (n, d)
      end
in
   fun inexact (FIX a) = flo (Real.fromInt a)
     | inexact (BIG a) = flo (Real.fromLargeInt a)
     | inexact (FLO a) = flo a
     | inexact (RAT (n, d)) = flo (ratioToReal (n, d))
     | inexact (CPX (a, b)) = cpx (inexact a, inexact b)
end

local
   (* Avoid exception handling for integer addition *)
   val fixAdd =
      case (Int.maxInt, Int.minInt) of
         (SOME max, SOME min) =>
            (fn (a, b) =>
               if a >= 0
                  then
                     if b > max - a
                        then BIG (IntInf.fromInt a + IntInf.fromInt b)
                     else fix (a + b)
               else
                  if b < min - a
                     then BIG (IntInf.fromInt a + IntInf.fromInt b)
                  else fix (a + b))
       | (NONE, NONE) => fix o op +
       | _ => raise Fail "Invalid Int.int bounds"

   (* Avoiding exception handling for multiplication requires division,
    * so use exception handling instead *)
   fun exactIntMul (FIX a, FIX b) = (fix (a * b)
      handle Overflow => big (IntInf.fromInt a * IntInf.fromInt b))
     | exactIntMul (FIX a, BIG b) = big (IntInf.fromInt a * b)
     | exactIntMul (BIG a, FIX b) = big (a * IntInf.fromInt b)
     | exactIntMul (BIG a, BIG b) = big (a * b)
     | exactIntMul _ = raise Fail "exactIntMul given non-exact-integer"
in
   fun add (FIX a, FIX b) = fixAdd (a, b)
     | add (FIX a, BIG b) = big (IntInf.fromInt a + b)
     | add (FIX a, FLO b) = flo (Real.fromInt a + b)
     | add (FIX a, RAT r) = add (RAT (FIX a, FIX 1), RAT r)
     | add (FIX a, CPX z) = add (CPX (FIX a, FIX 0), CPX z)

     | add (BIG a, FIX b) = big (a + IntInf.fromInt b)
     | add (BIG a, BIG b) = big (a + b)
     | add (BIG a, FLO b) = flo (Real.fromLargeInt a + b)
     | add (BIG a, RAT r) = add (RAT (BIG a, FIX 1), RAT r)
     | add (BIG a, CPX z) = add (CPX (BIG a, FIX 0), CPX z)

     | add (FLO a, FIX b) = flo (a + Real.fromInt b)
     | add (FLO a, BIG b) = flo (a + Real.fromLargeInt b)
     | add (FLO a, FLO b) = flo (a + b)
     | add (FLO a, RAT b) = raise Fail "NYI: add (FLO _, RAT _)"
     | add (FLO a, CPX z) = add (CPX (FLO a, FIX 0), CPX z)

     | add (RAT r, FIX b) = add (RAT r, RAT (FIX b, FIX 1))
     | add (RAT r, BIG b) = add (RAT r, RAT (BIG b, FIX 1))
     | add (RAT r, FLO b) = raise Fail "NYI: add (RAT _, FLO _)"
     | add (RAT (n, d), RAT (n', d')) =
      let
         val op * = exactIntMul
      in
         rat (add (n * d', n' * d), d * d')
      end
     | add (RAT r, CPX z) = add (CPX (RAT r, FIX 0), CPX z)

     | add (CPX z, FIX b) = add (CPX z, CPX (FIX b, FIX 0))
     | add (CPX z, BIG b) = add (CPX z, CPX (BIG b, FIX 0))
     | add (CPX z, FLO b) = add (CPX z, CPX (FLO b, FIX 0))
     | add (CPX z, RAT r) = add (CPX z, CPX (RAT r, FIX 0))
     | add (CPX (a, b), CPX (a', b')) = cpx (add (a, a'), add (b, b'))
end

fun realTrunc n = #whole (Real.split n)

fun realToBig n =
   let
      val sign = IntInf.fromInt (Real.sign n)

      fun go (n, scale, res) =
         if n < 1.0
            then sign * res
         else
            let
               val tmp = Real.trunc (Real.rem (n, 16.0))
               val res' = res + scale * IntInf.fromInt tmp
               val scale' = scale * 16
            in
               go (realTrunc (n / 16.0), scale', res')
            end
   in
      go (Real.abs n, 1, 0)
   end

fun realToRatio n = 
   if Real.== (n, realTrunc n)
      then BIG (realToBig n)
   else
      let
         val {whole, frac} = Real.split n
         val sign = IntInf.fromInt (Real.sign n)

         fun go (n, scale, res) =
            if Real.== (n, 0.0)
               then
                  add (rat (big (sign * res), big scale), big (realToBig whole))
            else
               let
                  val res' = res * IntInf.fromInt Real.radix
                  val scale' = scale * IntInf.fromInt Real.radix
                  val n' = n * Real.fromInt Real.radix
                  val i = Real.trunc n'
               in
                  if i <> 0
                     then go (n' - Real.fromInt i, scale', res' + IntInf.fromInt i)
                  else go (n', scale', res')
               end
      in
         go (Real.abs frac, 1, 0)
      end

fun exact (FIX n) = FIX n
  | exact (BIG n) = BIG n
  | exact (RAT (n, d)) = RAT (n, d)
  | exact (FLO n) =
   if not (Real.isFinite n)
      then raise Fail "SExpNum.exact: not a finite number"
   else if Real.!= (n, realTrunc n)
      then realToRatio n
   else if minInt <= n andalso n <= maxInt
      then FIX (Real.toInt IEEEReal.TO_ZERO n)
   else BIG (realToBig n)
  | exact (CPX (a, b)) = CPX (exact a, exact b)

fun mul (FIX a, FIX b) =
   (FIX (a * b) handle Overflow => BIG (IntInf.fromInt a * IntInf.fromInt b))
  | mul (BIG a, BIG b) = BIG (a * b)
  | mul (BIG a, FIX b) = BIG (a * IntInf.fromInt b)
  | mul (FIX a, BIG b) = BIG (IntInf.fromInt a * b)
  | mul _ = raise Fail "NYI"

fun compare (FIX a, FIX b) = Int.compare (a, b)
  | compare (BIG a, BIG b) = IntInf.compare (a, b)
  | compare (FLO a, FLO b) = Real.compare (a, b)
  | compare (FIX a, FLO b) =
   if Real.isFinite b
      then compare (FIX a, exact (FLO b))
   else if Real.isNan b
      then raise IEEEReal.Unordered
   else if b > 0.0 then LESS else GREATER
  | compare (CPX _, _) = raise Domain
  | compare (_, CPX _) = raise Domain
  | compare (FIX _, BIG b) = IntInf.compare (0, b)
  | compare (BIG a, FIX _) = IntInf.compare (a, 0)
  | compare _ = raise Fail "NYI"

val op + = add

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
