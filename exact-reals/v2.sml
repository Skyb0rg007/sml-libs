(* 0-dimensional linear fractional transformation
 * This is a homogeneous coordinate representation of an extended rational number
 * (Extended rational number ≡ ℚ ∪ {-∞,+∞})
 * a/b s.t. a ≠ 0 ∨ b ≠ 0
 * where a/b ≃ c/d if a*d = b*c
 *)

signature LFT =
  sig
    type t

    val numerator : t -> IntInf.int
    val denominator : t -> IntInf.int

    val posInf : t
    val negInf : t

    val + : t * t -> t
    val - : t * t -> t
    val * : t * t -> t
    val / : t * t -> t
    val ~ : t -> t
    val abs : t -> t

    val min : t * t -> t
    val max : t * t -> t

    val sign : t -> int

    val compare : t * t -> order
    val < : t * t -> bool
    val <= : t * t -> bool
    val > : t * t -> bool
    val >= : t * t -> bool
    val == : t * t -> bool
    val != : t * t -> bool

    val isFinite : t -> bool

    val fromInt : int -> t
    val fromIntInf : IntInf.int -> t
    val toString : t -> string
  end

structure Lft0 : LFT =
  struct
    nonfix div
    infix == !=

    (* Invariant: at least one of the integers are nonzero *)
    datatype t = T of IntInf.int * IntInf.int

    fun fromIntInf n = T (n, 1)
    val fromInt = fromIntInf o IntInf.fromInt

    val posInf = T (1, 0)
    val negInf = T (~1, 0)

    local
      fun gcd' (x, 0) = x
        | gcd' (x, y) = gcd' (y, IntInf.rem (x, y))

      fun gcd (x, y) = gcd' (IntInf.abs x, IntInf.abs y)

      fun reduce (0, 0) = raise Div
        | reduce (a, 0) = if a > 0 then posInf else negInf
        | reduce (0, _) = T (0, 1)
        | reduce (a, b) =
        let
          val d = gcd (a, b)
        in
          T (IntInf.quot (a, d), IntInf.quot (b, d))
        end
    in
      fun fromRat (a, b) =
        if b >= 0
          then reduce (a, b)
        else reduce (IntInf.~ a, IntInf.~ b)
    end

    fun numerator (T (a, _)) = a
    fun denominator (T (_, b)) = b

    fun lt (T (a, b), T (c, d)) = a * d < b * c
    fun le (T (a, b), T (c, d)) = a * d <= b * c
    fun gt (x, y) = lt (y, x)
    fun ge (x, y) = le (y, x)
    fun compare (T (a, b), T (c, d)) = IntInf.compare (a * d, b * c)
    fun (T (a, b)) == (T (c, d)) = a * d = b * c
    fun x != y = not (x == y)
    fun min (x, y) = if le (x, y) then x else y
    fun max (x, y) = if ge (x, y) then x else y
    val sign = IntInf.sign o numerator

    fun add (T (a, b), T (c, d)) = fromRat (a * d + b * c, b * d)
    fun sub (T (a, b), T (c, d)) = fromRat (a * d - b * c, b * d)
    fun mul (T (a, b), T (c, d)) = fromRat (a * c, b * d)
    fun div (T (a, b), T (c, d)) = fromRat (a * d, b * c)
    fun neg (T (a, b)) = fromRat (~a, b)
    fun abs (T (a, b)) = fromRat (IntInf.abs a, IntInf.abs b)
    fun recip (T (a, b)) = fromRat (IntInf.~ b, IntInf.~ a)
    fun isFinite x = denominator x <> 0

    fun toString (T (a, 1)) = IntInf.toString a
      | toString (T (a, 0)) = if a > 0 then "+inf" else "-inf"
      | toString (T (a, b)) = IntInf.toString a ^ "/" ^ IntInf.toString b

    val op < = lt
    val op <= = le
    val op > = gt
    val op >= = ge
    val op + = add
    val op - = sub
    val op * = mul
    val op / = div
    val ~ = neg
  end
