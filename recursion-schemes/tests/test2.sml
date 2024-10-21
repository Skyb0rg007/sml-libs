
structure Test2 =
struct

infixr 5 :<

fun maximum [] = 0.0
  | maximum (x::xs) = List.foldl Real.max x xs

fun knapsack1 wvs =
   let
      fun go c =
         maximum
         (List.mapPartial
            (fn (w, v) =>
               if 0 < w andalso w <= c
                  then SOME (v + go (c - w))
               else NONE)
            wvs)
   in
      go
   end

fun knapsack2 wvs =
   let
      val tbl = IntHashTable.mkTable (10, Subscript)

      fun go' c =
         maximum
         (List.mapPartial
            (fn (w, v) =>
               if 0 < w andalso w <= c
                  then SOME (v + go (c - w))
               else NONE)
            wvs)

      and go c =
         case IntHashTable.find tbl c of
            SOME x => x
          | NONE =>
            let
               val x = go' c
               val () = IntHashTable.insert tbl (c, x)
            in
               x
            end
   in
      go
   end

structure Susp = SMLofNJ.Susp

fun knapsack3 wvs n =
   let
      val default = Susp.delay (fn () => raise Fail "impossible")
      val arr = Array.array (n, default)
      fun go c =
         maximum
         (List.mapPartial
            (fn (w, v) =>
               if 0 < w andalso w <= c
                  then SOME (v + Susp.force (Array.sub (arr, c - w)))
               else NONE)
            wvs)
      val () = Array.modifyi (fn (i, _) => Susp.delay (fn () => go i)) arr
   in
      Susp.force (Array.sub (arr, n-1))
   end

val wvs = [(12, 4.0), (1, 2.0), (2, 2.0), (1, 1.0), (4, 10.0)]


structure NatF =
   struct
      datatype 'a t =
         Zero
       | Succ of 'a Susp.susp

      fun project 0 = Zero
        | project n =
         if n < 0
            then raise Domain
         else Succ (Susp.delay (fn () => n - 1))

      fun embed Zero = 0
        | embed (Succ n) = Susp.force n + 1

      fun map _ Zero = Zero
        | map f (Succ x) = Succ (Susp.delay (fn () => f (Susp.force x)))

      fun cata alg n = alg (map (cata alg) (project n))
   end

structure NatCofree =
   struct
      datatype 'a t = :< of 'a * 'a t NatF.t

      fun map f (x :< y) = f x :< NatF.map (map f) y
      fun extract (x :< _) = x
      fun duplicate (x :< y) = (x :< y) :< (NatF.map duplicate y)
      fun unwrap (_ :< y) = y

      fun lookup (x :< _, 0) = SOME x
        | lookup (_ :< NatF.Zero, _) = NONE
        | lookup (_ :< NatF.Succ y, n) = lookup (Susp.force y, n - 1)

      fun dist fc = NatF.map extract fc :< NatF.map (dist o unwrap) fc

      val _: 'a t NatF.t -> 'a NatF.t t = dist

      fun histo (alg: 'a t NatF.t -> 'a) (n: int): 'a =
         let
            fun go n = dist (NatF.map (duplicate o map alg o go) (NatF.project n))
         in
            alg (extract (go n))
         end
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
