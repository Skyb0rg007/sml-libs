
structure Pull :> PULL =
struct

datatype ('a, 's) step =
   Yield of 'a * 's
 | Skip of 's
 | Done

datatype ('a, 's) t = T of {
   seed: 's,
   step: 's -> ('a, 's) step,
   size: Size.t
}

val sizeZero = Size.fromInt 0

val empty = T {
   seed = (),
   step = fn () => Done,
   size = sizeZero
}

fun repeat x = T {
      seed = (),
      step = fn () => Yield (x, ()),
      size = Size.unknown
   }

fun fromList xs = T {
      seed = xs,
      step = fn x :: xs => Yield (x, xs) | [] => Done,
      size = Size.fromInt (List.length xs)
   }

fun fromVector v = T {
      seed = 0,
      step =
         fn i =>
            if i < Vector.length v
               then Yield (Vector.sub (v, i), i + 1)
            else Done,
      size = Size.fromInt (Vector.length v)
   }

fun unfold f s = T {
      seed = s,
      step =
         fn s =>
            case f s of
               NONE => Done
             | SOME (a, s') => Yield (a, s'),
      size = Size.unknown
   }

fun range (lo, hi) =
   let
      val (lo, hi) =
         if lo <= hi
            then (lo, hi)
         else (hi, lo)

      fun step n =
         if n >= hi
            then Done
         else Yield (n, n + 1)
   in
      T {
         seed = lo,
         step = step,
         size = Size.fromInt (Int.max (0, hi - lo))
      }
   end

fun countFrom n = T {
      seed = n,
      step = fn n => Yield (n, n + 1),
      size = Size.unknown
   }

fun zipWith f (stream1, stream2) =
   let
      val T {seed = seed1, step = step1, size = size1} = stream1
      val T {seed = seed2, step = step2, size = size2} = stream2

      fun step (s1, s2, NONE) =
         (case step1 s1 of
             Done => Done
           | Skip s1' => Skip (s1', s2, NONE)
           | Yield (a, s1') => Skip (s1', s2, SOME a))
        | step (s1, s2, SOME a) =
         case step2 s2 of
            Done => Done
          | Skip s2' => Skip (s1, s2', SOME a)
          | Yield (b, s2') => Yield (f (a, b), (s1, s2', NONE))
   in
      T {
         seed = (seed1, seed2, NONE),
         step = step,
         size = Size.min (size1, size2)
      }
   end

fun zipWith3 f (stream1, stream2, stream3) =
   let
      val T {seed = seed1, step = step1, size = size1} = stream1
      val T {seed = seed2, step = step2, size = size2} = stream2
      val T {seed = seed3, step = step3, size = size3} = stream3

      fun step (s1, s2, s3, NONE) =
         (case step1 s1 of
             Done => Done
           | Skip s1' => Skip (s1', s2, s3, NONE)
           | Yield (a, s1') => Skip (s1', s2, s3, SOME (a, NONE)))
        | step (s1, s2, s3, SOME (a, NONE)) =
         (case step2 s2 of
             Done => Done
           | Skip s2' => Skip (s1, s2', s3, SOME (a, NONE))
           | Yield (b, s2') => Skip (s1, s2', s3, SOME (a, SOME b)))
        | step (s1, s2, s3, SOME (a, SOME b)) =
         case step3 s3 of
            Done => Done
          | Skip s3' => Skip (s1, s2, s3', SOME (a, SOME b))
          | Yield (c, s3') => Yield (f (a, b, c), (s1, s2, s3', NONE))
   in
      T {
         seed = (seed1, seed2, seed3, NONE),
         step = step,
         size = Size.min (size1, Size.min (size2, size3))
      }
   end

fun map f (T {seed, step, size}) = T {
      seed = seed,
      step =
         fn s =>
            case step s of
               Done => Done
             | Skip s' => Skip s'
             | Yield (a, s') => Yield (f a, s'),
      size = size
   }

fun take n (T {seed, step, size}) =
   let
      fun step' (s, n) =
         if n <= 0
            then Done
         else 
            case step s of
               Done => Done
             | Skip s' => Skip (s', n)
             | Yield (a, s') => Yield (a, (s', n - 1))
   in
      T {
         seed = (seed, n),
         step = step',
         size = Size.max (Size.fromInt n, size)
      }
   end

fun drop n (T {seed, step, size}) =
   let
      fun dropState (n, s) =
         if n <= 0
            then s
         else
            case step s of
               Done => s
             | Skip s' => dropState (n, s')
             | Yield (_, s') => dropState (n - 1, s')
   in
      T {
         seed = dropState (n, seed),
         step = step,
         size = Size.- (size, Size.fromInt n)
      }
   end

fun filter f (T {seed, step, size}) =
   let
      fun step' s =
         case step s of
            Done => Done
          | Skip s' => Skip s'
          | Yield (a, s') =>
               if f a
                  then Yield (a, s')
               else Skip s'
   in
      T {
         seed = seed,
         step = step',
         size = Size.toMax size
      }
   end

fun foldl f z (T {seed, step, ...}) =
   let
      fun go (s, acc) =
         case step s of
            Done => acc
          | Skip s' => go (s', acc)
          | Yield (a, s') => go (s', f (a, acc))
   in
      go (seed, z)
   end

fun foldr f z (T {seed, step, ...}) =
   let
      fun go s =
         case step s of
            Done => z
          | Skip s' => go s'
          | Yield (a, s') => f (a, go s')
   in
      go seed
   end

fun toList s = foldr op :: [] s

fun uncons (T {seed, step, size}) =
   let
      fun go s =
         case step s of
            Done => NONE
          | Skip s' => go s'
          | Yield (a, s') =>
               let
                  val stream = T {
                        seed = s',
                        step = step,
                        size = Size.- (size, Size.fromInt 1)
                     }
               in
                  SOME (a, stream)
               end
   in
      go seed
   end

(* TODO: Optimize for MLton's Vector.unfoldli *)
fun toVector s = Vector.fromList (toList s)

fun length (s as T {size, ...}) =
   case Size.exact size of
      SOME n => n
    | NONE => foldl (fn (_, n) => n + 1) 0 s

fun size (T {size, ...}) = size

fun unsafeSized (T {seed, step, ...}, size) =
   T {seed = seed, step = step, size = size}

fun toArray (s as T {seed, step, size}) =
   case Size.exact size of
      NONE => Array.fromList (toList s)
    | SOME 0 => Array.fromList []
    | SOME n =>
         let
            val state = ref seed
            fun next () =
               case step (!state) of
                  Done => raise Fail "Size mismatch"
                | Skip s' => (state := s'; next ())
                | Yield (a, s') => (state := s'; a)

            val arr = Array.array (n, next ())

            fun go i =
               if i < n
                  then (Array.update (arr, i, next ()); go (i + 1))
               else ()

            val () = go 1
         in
            arr
         end

end

(* vim: set tw=0 ts=3 sw=3: *)
