
structure Literal =
struct
   structure Number =
      struct
         datatype t =
            Fixnum of int
          | Flonum of real
          | Bignum of IntInf.int
          | Ratnum of t * t
          | Cpxnum of t * t

         fun isZero (Fixnum n) = n = 0
           | isZero (Flonum n) = Real.== (n, 0.0)
           | isZero (Bignum n) = n = 0
           | isZero (Ratnum (n, d)) = isZero n
           | isZero (Cpxnum (a, b)) = isZero a andalso isZero b

         fun toString (Fixnum n) = 
            if n < 0
               then "-" ^ Int.toString (~n)
            else Int.toString n
           | toString (Flonum n) =
            if Real.isFinite n
               then Real.fmt StringCvt.EXACT n
            else if Real.isNan n
               then "+nan.0"
            else if n > 0
               then "+inf.0"
            else "-inf.0"
           | toString (Bignum n) =
            if n < 0
               then "-" ^ IntInf.toString (~n)
            else IntInf.toString n
           | toString (Ratnum (n, d)) =
            String.concat [toString n, "/", toString d]
           | toString (Cpxnum (a, b)) =
            "cpx"
      end

   (* datatype t = *)
   (*    Int of IntInf.int *)
   (*  | *) 
end

(* vim: set tw=0 sw=3 ts=3: *)
