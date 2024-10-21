
signature FUNCTOR =
   sig
      type 'a t
      val map: ('a -> 'b) -> 'a t -> 'b t
   end

signature APPLICATIVE =
   sig
      include FUNCTOR
      val pure: 'a -> 'a t
      val ap: ('a -> 'b) t * 'a t -> 'b t
   end

signature PREDICTABLE =
   sig
      include FUNCTOR
      val predict: 'a t Later.t -> 'a Later.t t
   end

signature PREDICTABLE_APPLICATIVE =
   sig
      include APPLICATIVE
      val predict: 'a t Later.t -> 'a Later.t t
   end

structure Delay =
   struct
      datatype 'a t =
         Now of 'a
       | Wait of 'a t Later.t
   end

structure Predictable:
   sig
      val predictFunc: ('a -> 'b) Later.t -> 'a -> 'b Later.t
      val predictProd: ('a * 'b) Later.t -> 'a Later.t * 'b Later.t
   end =
   struct
      fun predictFunc x y = Later.map (fn k => k y) x
      fun predictProd (x: ('a * 'b) Later.t) = (Later.map #1 x, Later.map #2 x)
   end

structure Stream:
   sig
      datatype 'a t =
         Nil
       | Cons of 'a * 'a t Later.t

      val interleave: 'a t * 'a t -> 'a t
      val zip: 'a t * 'b t -> ('a * 'b) t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val foldr: ('a * 'b Later.t -> 'b) -> 'b -> 'a t -> 'b
      val last: 'a t -> 'a option Delay.t
      functor ITraverse(A: PREDICTABLE_APPLICATIVE):
         sig
            val isequence: 'a A.t t -> 'a t A.t
            val ibackquence: 'a A.t t -> 'a t A.t
         end
   end =
   struct
      datatype 'a t =
         Nil
       | Cons of 'a * 'a t Later.t

      fun map _ Nil = Nil
        | map f (Cons (x, xs)) = Cons (f x, Later.map (map f) xs)

      fun foldr f z =
         Later.fix (fn recur =>
            fn Nil => z
             | Cons (x, xs) => f (x, Later.map2 Fn.apply (recur, xs)))

      fun interleave args =
         Later.fix (fn recur =>
            fn (Nil, s2) => s2
             | (Cons (x, xs), s2) => Cons (x, Later.ap2 (recur, Later.pure s2, xs)))
            args

      fun zip (s1, s2) =
         Later.fix (fn recur =>
            fn (Cons (x, xs), Cons (y, ys)) =>
                  Cons ((x, y), Later.ap2 (recur, xs, ys))
             | (_, _) => Nil)
            (s1, s2)

      fun last s =
         let
            fun go _ (def, Nil) = Delay.Now def
              | go recur (def, Cons (x, xs)) =
               Delay.Wait (Later.ap2 (recur, Later.pure (SOME x), xs))
         in
            Later.fix go (NONE, s)
         end

      functor ITraverse(A: PREDICTABLE_APPLICATIVE) =
         struct
            fun cons (x, xs) = A.ap (A.map (Fn.curry Cons) x, xs)
            fun isequence s =
               Later.fix (fn recur =>
                  fn Nil => A.pure Nil
                   | Cons (x, xs) => cons (x, A.predict (Later.ap (recur, xs))))
                  s

            fun cons' (xs, x) = A.ap (A.map (Fn.curry (Fn.flip Cons)) xs, x)
            fun ibackquence s =
               Later.fix (fn recur =>
                  fn Nil => A.pure Nil
                   | Cons (x, xs) => cons' (A.predict (Later.ap (recur, xs)), x))
                  s
         end
   end

structure Test =
   struct

      val nats: int Stream.t =
         Later.fix (fn recur =>
            Stream.Cons (0, Later.map (Stream.map (fn n => n + 1)) recur))

      (* fun take _ Stream.Nil = [] *)
      (*   | take n (Stream.Cons (x, xs)) = *)
      (*    if n <= 0 *)
      (*       then [] *)
      (*    else x :: take (n - 1) xs *)
   end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
