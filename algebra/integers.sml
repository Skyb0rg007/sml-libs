
signature RATIONAL =
   sig
      type t
      type int

      val make: int * int -> t

      val + : t * t -> t
      val - : t * t -> t
      val * : t * t -> t
      val / : t * t -> t
      val ~ : t -> t
      val abs: t -> t
      val sign: t -> Int.int
      val fromInt: Int.int -> t
      val fromLargeInt: IntInf.int -> t

      val <= : t * t -> bool
      val < : t * t -> bool
      val >= : t * t -> bool
      val > : t * t -> bool
      val compare: t * t -> order
      val min: t * t -> t
      val max: t * t -> t

      val toString: t -> string
   end

functor RationalFn(I : INTEGER) : RATIONAL =
   struct
      datatype t = T of I.int * I.int
      type int = I.int

      fun make (p, q) =
         let
            val () = if I.sign q = 0 then raise Div else ()
            fun gcd (a, b) =
               if I.sign b = 0
                  then a
                  else gcd (b, I.rem (a, b))
            val d = gcd (I.abs p, I.abs q)
         in
            T (I.quot (p, d), I.quot (q, d))
         end

      fun (T (p1, q1)) + (T (p2, q2)) = make (I.+ (I.* (p1, q2), I.* (p2, q1)), I.* (q1, q2))
      fun (T (p1, q1)) - (T (p2, q2)) = make (I.- (I.* (p1, q2), I.* (p2, q1)), I.* (q1, q2))
      fun (T (p1, q1)) * (T (p2, q2)) = make (I.* (p1, p2), I.* (q1, q2))
      fun (T (p1, q1)) / (T (p2, q2)) = make (I.* (p1, q2), I.* (p2, q1))

      fun (T (p1, q1)) <= (T (p2, q2)) = I.<= (I.* (p1, q2), I.* (p2, q1))
      fun (T (p1, q1)) < (T (p2, q2)) = I.< (I.* (p1, q2), I.* (p2, q1))
      fun compare (T (p1, q1), T (p2, q2)) = I.compare (I.* (p1, q2), I.* (p2, q1))
      fun a > b = b < a
      fun a >= b = b <= a
      fun min (a, b) = if a <= b then a else b
      fun max (a, b) = if a >= b then a else b

      fun ~ (T (p, q)) = T (I.~ p, q)
      fun abs (T (p, q)) = T (I.abs p, q)
      fun sign (T (p, _)) = I.sign p
      fun fromInt n = T (I.fromInt n, I.fromInt 1)
      fun fromLargeInt n = T (I.fromLarge n, I.fromInt 1)

      fun toString (T (p, q)) = String.concat [I.toString p, "/", I.toString q]
   end

structure Rational = RationalFn(Int)

(* vim: set ts=3 sw=3 :*)

