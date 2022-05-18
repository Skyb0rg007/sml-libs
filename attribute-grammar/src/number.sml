
structure Number =
struct
   datatype t =
      FIXNUM of int
    | FLONUM of real
    | BIGNUM of IntInf.int
    | RATNUM of t * t
    | CPXNUM of t * t

   val bignum =
      case (Int.minInt, Int.maxInt) of
         (NONE, NONE) => FIXNUM o LargeInt.toInt
       | (SOME min, SOME max) =>
            let
               val min = IntInf.fromInt min
               val max = IntInf.fromInt max
            in
               fn n =>
                  if min <= n andalso n <= max
                     then FIXNUM (Int.fromLarge n)
                  else BIGNUM n
            end
       | _ => raise Fail "Int has minInt but no maxInt, or vice versa"

   fun realToRatio n =
      if Real.== (n, Real.realTrunc n)
         then bignum (Real.toLargeInt IEEEReal.TO_NEAREST n)
      else
         raise Option

   fun exact (n as FIXNUM _) = n
     | exact (n as BIGNUM _) = n
     | exact (n as RATNUM _) = n
     | exact (FLONUM n) =
      if Real.isFinite n
         then realToRatio n
      else raise Overflow

   fun compare (CPXNUM _, _) = raise Domain
     | compare (_, CPXNUM _) = raise Domain
     | compare (FIXNUM n, FIXNUM m) = Int.compare (n, m)
     | compare (FIXNUM n, FLONUM m) =
      if Real.isNan m
         then raise IEEEReal.Unordered
      else if Real.isFinite m
         then compare (FIXNUM n, exact (FLONUM m))
      else if m > 0.0
         then LESS
      else GREATER

   fun isNegative (FIXNUM n) = n < 0
     | isNegative (FLONUM n) = n < 0.0
     | isNegative (BIGNUM n) = n < 0
     | isNegative (RATNUM (n, _)) = isNegative n
     | isNegative (CPXNUM _) = raise Domain

   fun isPositive (FIXNUM n) = n > 0
     | isPositive (FLONUM n) = n > 0.0
     | isPositive (BIGNUM n) = n > 0
     | isPositive (RATNUM (n, _)) = isPositive n
     | isPositive (CPXNUM _) = raise Domain

   fun isExactOne (FIXNUM 1) = true
     | isExactOne _ = false

   fun isExactNegOne (FIXNUM ~1) = true
     | isExactNegOne _ = false

   fun toString (FLONUM n) =
      if Real.isNan n
         then "+nan.0"
      else if not (Real.isFinite n)
         then if n > 0.0 then "+inf.0" else "-inf.0"
      else if n >= 0.0
         then Real.fmt (StringCvt.GEN (SOME 15)) n
      else "-" ^ Real.fmt (StringCvt.GEN (SOME 15)) (~n)
     | toString (FIXNUM n) =
      if n >= 0
         then Int.toString n
      else "-" ^ Int.toString (~n)
     | toString (BIGNUM n) =
      if n >= 0
         then IntInf.toString n
      else "-" ^ IntInf.toString (~n)
     | toString (RATNUM (n, d)) =
      String.concat [toString n, "/", toString d]
     | toString (CPXNUM (a, b)) =
      String.concat
         [toString a,
          if isNegative b
             then ""
          else "+",
          case b of
             FIXNUM 1 => "i"
           | FIXNUM ~1 => "-i"
           | _ => toString b ^ "i"]
end

(* vim: set ft=sml ts=3 sw=3 tw=0: *)
