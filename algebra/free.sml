
(* Free construction for a binary operator *)
functor FreeMagma(type t) : MAGMA =
   struct
      type s = t
      datatype t = Free of s
                 | Mult of t * t

      val free = Free
      val op * = Mult
   end

(* Free construction for an associative binary operator *)
functor FreeSemigroup(type t) : MAGMA =
   struct
      type s = t
      datatype t = T of s * s list

      fun free x = T (x, [])

      fun (T (x, xs)) * (T (y, ys)) = T (x, xs @ y :: ys)
   end

(* Add an identity element to a binary operator *)
functor FreeUnitalMagma(M : MAGMA) : UNITAL_MAGMA =
   struct
      type t = M.t option

      val free = SOME
      val one = NONE
      fun NONE * y = y
        | x * NONE = x
        | (SOME x) * (SOME y) = SOME (M.* (x, y))
   end

functor FreeRightQuasigroup(type t) : RIGHT_QUASIGROUP =
   struct
      type s = t
      datatype t = Free of s
                 | Mult of t * t
                 | RDiv of t * t

      val free = Free
      val op * = Mult
      val op / = RDiv
   end

(* Concrete Examples *)

functor IntegerSubQuasigroupFn(I : INTEGER) : QUASIGROUP where type t = I.int =
   struct
      type t = I.int

      (* [x * (x \ y)] = x - (x - y) = y
       * [x \ (x * y)] = x - (x - y) = y
       * [(y / x) * x] = (y + x) - x = y
       * [(y * x) / x] = (y - x) + x = y
       *)
      val op * = I.-
      val op / = I.+
      val op \ = I.-
   end

structure IntSubQuasigroup = IntegerSubQuasigroupFn(Int)
structure IntInfSubQuasigroup = IntegerSubQuasigroupFn(IntInf)

functor RationalSubQuasigroupFn(R : RATIONAL) : QUASIGROUP =
   struct
      type t = R.t

      val op * = R.-
      val op / = R.+
      val op \ = R.-
   end

structure RatSubQuasigroup = RationalSubQuasigroupFn(Rational)

functor RationalDivQuasigroup(R : RATIONAL) : QUASIGROUP =
   struct
      infix 7 \

      type t = R.t

      (* [x * (x \ y)] = x ÷ (y ÷ x) = y
       * [x \ (x * y)] = (x ÷ y) ÷ x = y
       * [(y / x) * x] = (y * x) ÷ x = y
       * [(y * x) / x] = (y ÷ x) * x = y
       *)
      val op * = R./
      val op / = R.*
      fun a \ b = R./ (b, a)
   end

structure RatDivQuasigroup = RationalDivQuasigroup(Rational)

(* vim: set ts=3 sw=3 :*)
