
structure LftReal =
struct
  structure V =
    struct
      datatype t = T of IntInf.int * IntInf.int

      val make = T

      fun sign (T (a, b)) =
        case IntInf.compare (a, 0) of
            LESS => if b > 0 then 0 else ~1
          | GREATER => if b < 0 then 0 else 1
          | EQUAL =>
            case IntInf.compare (b, 0) of
                LESS => ~1
              | GREATER => 1
              | EQUAL => 0

      fun refine v = sign v <> 0

      fun scale (T (a, b)) =
        let
          val (aq, ar) = IntInf.divMod (a, 2)
          val (bq, br) = IntInf.divMod (b, 2)
        in
          if ar = 0 andalso br = 0
            then scale (T (aq, bq))
          else T (a, b)
        end

      fun less (T (a, b), T (c, d)) = a * d - b * c < 0
    end

  structure M =
    struct
      datatype t = T of V.t * V.t

      fun make (a, b, c, d) = T (V.make (a, b), V.make (c, d))

      val identity = T (V.T (1, 0), V.T (0, 1))

      val spos = make (1,  0,  0, 1)
      val sinf = make (1, ~1,  1, 1)
      val sneg = make (0,  1, ~1, 0)
      val szer = make (1,  1, ~1, 1)
      val dneg = make (1,  1,  0, 2)
      val dzer = make (3,  1,  1, 3)
      val dpos = make (2,  0,  1, 1)

      fun determinant (T (V.T (a, b), V.T (c, d))) = a * d - b * c

      local
        fun scale' (a, b, c, d) =
          let
            val (aq, ar) = IntInf.divMod (a, 2)
            val (bq, br) = IntInf.divMod (b, 2)
            val (cq, cr) = IntInf.divMod (c, 2)
            val (dq, dr) = IntInf.divMod (d, 2)
          in
            if ar = 0 andalso br = 0 andalso cr = 0 andalso dr = 0
              then scale' (aq, bq, cq, dq)
            else make (a, b, c, d)
          end
      in
        fun scale (T (V.T (a, b), V.T (c, d))) = scale' (a, b, c, d)

        fun inverse (T (V.T (a, b), V.T (c, d))) = scale' (d, ~b, ~c, a)
      end

      fun mul (T (V.T (a, b), V.T (c, d)), V.T (e, f)) =
        V.make (a * e + c * f, b * e + d * f)

      fun dot (m, T (u, v)) = T (mul (m, u), mul (m, v))

      fun refine (T (u, v)) =
        let
          val s1 = V.sign u
          val s2 = V.sign v
        in
          s1 = s2 andalso s2 <> 0
        end

      fun lessV (T (u, v), x) = V.less (u, x) andalso V.less (v, x)

      fun less (m, T (u, v)) = lessV (m, u) andalso lessV (m, v)

      fun disjoint (m, n) = less (m, n) orelse less (n, m)

      local
        fun mantissa (i, n, m) =
          if refine (dot (inverse (T (V.T (n+1, 10), V.T (n-1, 10))), m))
            then i :: mantissa (~9, 9, dot (make (10, 0, ~n, 1), m))
          else if i < n
            then mantissa (i + 1, n, m)
          else []

        fun scientific (m, n) =
          if V.refine (mul (inverse m, V.T (1, 0)))
            then []
          else if refine (dot (inverse szer, m))
            then n :: mantissa (~9, 9, m)
          else scientific (dot (T (V.T (1, 0), V.T (0, 10)), m), n + 1)

        fun normalize (e : IntInf.int, l, v) =
          if l > 0 andalso IntInf.abs v < IntInf.pow (10, l - 1)
            then normalize (e - 1, l - 1, v)
          else (e, l, v)

        fun showSign (v : IntInf.int) = if v < 0 then "-" else ""

        fun showM v =
          if v = 0
            then "0"
          else "0." ^ IntInf.toString (IntInf.abs v)

        fun showE e =
          "e" ^ IntInf.toString e

        fun sshow [] = "unbounded"
          | sshow (e :: m) =
          let
            fun g (d, c) = d + 10 * c
            fun f x = List.foldl g 0 x
            val (h, l, v) = normalize (e, List.length m, f m)
          in
            showSign v ^ showM v ^ showE h
          end
      in
        fun toString (m as T (u, _)) =
          let
            val d = determinant m
            val V.T (a, b) = V.scale u
            val (p, q) = if b < 0 then (~a, ~b) else (a, b)
          in
            if d = 0
              then
                if q = 1
                  then IntInf.toString p
                else IntInf.toString p ^ "/" ^ IntInf.toString q
            else sshow (scientific (m, 0))
          end
      end
    end

  structure T =
    struct
      datatype t = T of M.t * M.t

      fun make (a, b, c, d, e, f, g, h) =
        T (M.make (a, b, c, d), M.make (e, f, g, h))

      val tdiv = make (0, 0, 1, 0, 0, 1, 0, 0)
      val tsub = make (0, 0, 1, 0, ~1, 0, 0, 1)
    end

  structure E =
    struct
      datatype node
        = ExpV of V.t * t
        | ExpM of M.t * t
        | ExpT of T.t * IntInf.int * t * t

      withtype t = unit -> node

      fun erec e () = ExpM (M.make (0, 1, 1, 0), e)

      fun neg e () = ExpM (M.make (~1, 0, 0, 1), e)

      fun iterate i (n : IntInf.int) () = ExpM (i n, iterate i (n + 1))

      fun rollover (a, b, c) () =
        let
          val d = 2 * (b - a) + c
        in
          if d >= 0
            then ExpM (M.dneg, rollover (4 * a, d, c))
          else ExpM (M.dpos, rollover (~d, 4 * b, c))
        end

      fun sqrtrat (p, q) = rollover (p, q, p - q)

      local
        fun iteromega 0 = M.make (6795705, 213440, 6795704, 213440)
          | iteromega n =
          let
            val b = (2 * n - 1) * (6 * n - 5) * (6 * n - 1)
            val c = b * (545140134 * n + 13591409)
            val d = b * (n + 1)
            val e = 10939058860032000 * IntInf.pow (n, 4)
          in
            M.make (e - d - c, e + d + c, e + d - c, e - d + c)
          end
      in
        val omega = iterate iteromega 0
      end

      fun pi () = ExpT (T.tdiv, 0, sqrtrat (10005, 1), omega)
    end
end
