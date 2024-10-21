
structure Complex =
struct
  datatype t = T of IntInf.int * IntInf.int

  fun fromInt n = T (n, 0)

  val i = T (0, 1)

  fun add (T (a, b), T (c, d)) = T (a + c, b + d)

  fun sub (T (a, b), T (c, d)) = T (a - c, b - d)

  fun conj (T (a, b)) = T (a, ~b)

  fun mul (T (a, b), T (c, d)) =
    T (a * c - b * d, b * c + a * d)

  val op + = add
  val op - = sub
  val op * = mul
end

structure EComplex =
struct
  nonfix div

  (* Invariant: At least one complex number is nonzero *)
  datatype t = T of Complex.t * Complex.t

  fun fromComplex z = T (z, Complex.fromInt 1)
  fun fromInt n = fromComplex (Complex.fromInt n)

  local
    val op * = Complex.*
    val op + = Complex.+
    val op - = Complex.-
  in
    fun add (T (a, b), T (c, d)) = T (a * d + b * c, b * d)

    fun sub (T (a, b), T (c, d)) = T (a * d - b * c, b * d)

    fun mul (T (a, b), T (c, d)) = T (a * c, b * d)

    fun div (T (a, b), T (c, d)) = T (a * d, b * c)
  end

  val op + = add
  val op - = sub
  val op * = mul
  val op / = div
end
