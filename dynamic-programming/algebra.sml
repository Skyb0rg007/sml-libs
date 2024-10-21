
structure BooleanAlgebra: KLEENE_ALGEBRA =
   struct
      type t = bool

      val zero = false
      val one = true
      fun a + b = a orelse b
      fun a * b = a andalso b
      fun closure _ = true
   end

functor TupleMonoid(structure A: MONOID structure B: MONOID): MONOID
      where type t = A.t * B.t =
   struct
      type t = A.t * B.t

      val one = (A.one, B.one)
      fun (a, b) * (c, d) = (A.* (a, c), B.* (b, d))
   end

functor ListMonoid(type t): MONOID where type t = t list =
   struct
      type t = t list

      val one = []
      val op * = List.@
   end

signature ORDERED_MONOID =
   sig
      include MONOID

      val compare: t * t -> order
   end

functor TropicalSemiring(M: ORDERED_MONOID): STAR_SEMIRING
      where type t = M.t option =
   struct
      type t = M.t option

      val zero = NONE
      val one = SOME M.one

      fun NONE + y = y
        | x + NONE = x
        | (SOME x) + (SOME y) = 
         if M.compare (x, y) = GREATER
            then SOME y
            else SOME x

      fun NONE * _ = NONE
        | _ * NONE = NONE
        | (SOME x) * (SOME y) = SOME (M.* (x, y))

      fun closure _ = one

      (* fun compare (SOME x, SOME y) = M.compare (x, y) *)
      (*   | compare (NONE, NONE) = EQUAL *)
      (*   | compare (NONE, SOME _) = GREATER *)
      (*   | compare (SOME _, NONE) = LESS *)
   end

signature MATRIX =
   sig
      type 'a t

      val size: int
      val matrix: (int * int -> 'a) -> 'a t
      val adjacency: 'a -> ((int * int) * 'a) list -> 'a t
      val sub: 'a t * int * int -> 'a
      val map: ('a -> 'b) -> 'a t -> 'b t
      val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
      val pure: 'a -> 'a t
      val apply: ('a -> 'b) t * 'a t -> 'b t
   end

functor Matrix(val n : int): MATRIX =
   struct
      datatype 'a t = T of 'a vector

      val size = n

      fun matrix f = T (Vector.tabulate (n * n, fn i => f (i div n, i mod n)))

      fun adjacency default xs =
         let
            fun init i =
               case List.find (fn (j, _) => i = j) xs of
                  NONE => default
                | SOME (_, x) => x
         in
            matrix init
         end

      fun sub (T v, i, j) =
         if i < 0 orelse j < 0 orelse i >= n orelse j >= n
            then raise Subscript
            else Vector.sub (v, n * i + j)

      fun map f (T v) = T (Vector.map f v)

      fun pure x = T (Vector.tabulate (n * n, fn _ => x))

      fun apply (T f, T x) =
         let
            fun init i = Vector.sub (f, i) (Vector.sub (x, i))
         in
            T (Vector.tabulate (n * n, init))
         end

      fun map2 f (T x, T y) =
         let
            fun init i = f (Vector.sub (x, i), Vector.sub (y, i))
         in
            T (Vector.tabulate (n * n, init))
         end
   end

functor MatrixSemiring(structure R: STAR_SEMIRING structure M: MATRIX): STAR_SEMIRING
      where type t = R.t M.t =
   struct
      type t = R.t M.t

      val zero = M.pure R.zero

      val one = M.matrix (fn (i, j) => if i = j then R.one else R.zero)

      val op + = M.map2 R.+

      fun a * b =
         let
            fun init (s, t) =
               let
                  fun go (i, acc) =
                     if i < M.size
                        then go (Int.+ (i, 1), R.+ (acc, R.* (M.sub (a, s, i), M.sub (b, i, t))))
                        else acc
               in
                  go (0, R.zero)
               end
         in
            M.matrix init
         end

      fun closure a =
         let
            fun step (k, m) =
               M.matrix (fn (i, j) =>
                  R.+ (M.sub (m, i, j),
                       R.* (M.sub (m, i, k),
                            R.* (R.closure (M.sub (m, k, k)),
                                 M.sub (m, k, j)))))

            fun go (i, acc) =
               if i < M.size
                  then go (Int.+ (i, 1), step (i, acc))
                  else acc
         in
            one + go (0, a)
         end
   end

functor RegexSemiring(type t) =
   struct
      type atom = t
      datatype t =
         Empty
       | Eps
       | Atom of atom
       | Cat of t * t
       | Alt of t * t
       | Rep of t

      val zero = Empty

      val one = Eps

      fun Empty + y = y
        | x + Empty = x
        | Eps + (Rep y) = Rep y
        | (Rep x) + Eps = Rep x
        | Eps + Eps = Eps
        | x + y = Alt (x, y)

      fun Empty * _ = Empty
        | _ * Empty = Empty
        | Eps * y = y
        | x * Eps = x
        | x * y = Cat (x, y)

      fun closure Empty = Eps
        | closure Eps = Eps
        | closure (Rep x) = Rep x
        | closure x = Rep x

      fun interpret {zero, one, add, mul, closure, atom} =
         let
            fun loop Empty = zero
              | loop Eps = one
              | loop (Atom a) = atom a
              | loop (Alt (x, y)) = add (loop x) (loop y)
              | loop (Cat (x, y)) = mul (loop x) (loop y)
              | loop (Rep x) = closure (loop x)
         in
            loop
         end
   end

structure RealField =
   struct
      datatype t = REAL of real | EXTRA

      val zero = REAL 0.0
      val one = REAL 1.0

      fun EXTRA + _ = EXTRA
        | _ + EXTRA = EXTRA
        | (REAL x) + (REAL y) = REAL (Real.+ (x, y))

      fun is_zero EXTRA = false
        | is_zero (REAL x) = Real.== (x, 0.0)

      fun x * y =
         if is_zero x orelse is_zero y
            then REAL 0.0
         else
            case (x, y) of
               (EXTRA, _) => EXTRA
             | (_, EXTRA) => EXTRA
             | (REAL x, REAL y) => REAL (Real.* (x, y))

      fun closure EXTRA = EXTRA
        | closure (REAL x) =
         if Real.== (x, 1.0)
            then EXTRA
            else REAL (Real./ (1.0, 1.0 - x))
   end

(* vim: set tw=0 ts=3 sw=3: *)
