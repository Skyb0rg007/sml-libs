
functor TambaraSum(P: PROFUNCTOR): CHOICE =
   struct
      datatype either = datatype Either.either
      type ('a, 'b) t = (('a, exn) either, ('b, exn) either) P.t

      fun dimap f g = P.dimap (Either.mapLeft f) (Either.mapLeft g)
      fun lmap f = P.lmap (Either.mapLeft f)
      fun rmap g = P.rmap (Either.mapLeft g)

      fun left (p: ('a, 'b) t): (('a, 'c) either, ('b, 'c) either) t =
         let
            exception E of ('c, exn) either
            fun hither (INL (INL x)) = INL x
              | hither (INL (INR y)) = INR (E (INL y))
              | hither (INR z) = INR (E (INR z))
            fun yon (INL x) = INL (INL x)
              | yon (INR (E (INL y))) = INL (INR y)
              | yon (INR (E (INR z))) = INR z
              | yon (INR _) = raise Fail "impossible"
         in
            P.dimap hither yon p
         end

      local
         fun swap (INL x) = INR x
           | swap (INR y) = INL y
      in
         fun right p = dimap swap swap (left p)
      end
   end

functor PastroSum(P: PROFUNCTOR): CHOICE =
   struct
      datatype either = datatype Either.either
      datatype ('a, 'b) t = T of
           ((y, z) either -> 'b)
         * (x, y) P.t
         * ('a -> (x, z) either)
      withtype x = exn and y = exn and z = exn

      fun dimap f g (T (l, m, r)) = T (g o l, m, r o f)
      fun lmap f (T (l, m, r)) = T (l, m, r o f)
      fun rmap g (T (l, m, r)) = T (g o l, m, r)

      fun left (T (l, m, r): ('a, 'b) t): (('a, 'c) either, ('b, 'c) either) t =
         let
            exception E of (exn, 'c) either
            fun l' (INL y) = INL (l (INL y))
              | l' (INR (E (INL z))) = INL (l (INR z))
              | l' (INR (E (INR c))) = INR c
              | l' (INR _) = raise Fail "impossible"
            fun r' (INL x) = Either.mapRight (E o INL) (r x)
              | r' (INR y) = INR (E (INR y))
         in
            T (l', m, r')
         end

      local
         fun swap (INL x) = INR x
           | swap (INR y) = INL y
      in
         fun right p = dimap swap swap (left p)
      end
   end

structure TambaraTagged = TambaraSum(Tagged)
structure PastroTagged = PastroSum(Tagged)

(* vim: set tw=0 ts=3 sw=3: *)
