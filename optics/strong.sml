
functor Tambara(P: PROFUNCTOR): STRONG =
   struct
      type ('a, 'b) t = ('a * exn, 'b * exn) P.t

      fun make (p: ('a * 'c, 'b * 'c) P.t): ('a, 'b) t =
         let
            exception E of 'c
            fun to (b, E c) = (b, c)
              | to _ = raise Fail "impossible"
            fun from (a, c) = (a, E c)
         in
            P.dimap to from p
         end

      fun dimap f g = P.dimap (fn (a, c) => (f a, c)) (fn (b, c) => (g b, c))
      fun lmap f = P.lmap (fn (a, c) => (f a, c))
      fun rmap g = P.rmap (fn (b, c) => (g b, c))

      fun first (p: ('a, 'b) t): ('a * 'c, 'b * 'c) t =
         let
            exception E of 'c * exn
            fun hither ((x, y), z) = (x, E (y, z))
            fun yon (x, E (y, z)) = ((x, y), z)
              | yon _ = raise Fail "impossible"
         in
            P.dimap hither yon p
         end

      local
         fun swap (x, y) = (y, x)
      in
         fun second p = dimap swap swap (first p)
      end

      fun uncurry p = rmap Fn.apply (first p)
      fun strong f p = dimap (fn a => (a, a)) f (second p)
   end

(* vim: set tw=0 ts=3 sw=3: *)
