
structure Push: PUSH =
struct

type 'a thunk = unit -> 'a

datatype ('a, 's) t = T of {
   fold: ('a * 's thunk -> 's) * 's thunk -> 's,
   size: Size.t
}

val sizeZero = Size.fromInt 0

val empty =
   T {
      fold = fn (_, n) => n (),
      size = sizeZero
   }

fun cons (x, T {fold, size}) =
   T {
      fold = fn (c, n) => c (x, fn () => fold (c, n)),
      size = Size.+ (size, Size.fromInt 1)
   }

fun singleton x =
   T {
      fold = fn (c, n) => c (x, n),
      size = Size.fromInt 1
   }

fun repeat x =
   T {
      fold =
         fn (c, _) =>
            let
               fun go () = c (x, go)
            in
               go ()
            end,
      size = Size.unknown
   }

fun head (T {fold, ...}) = fold (fn (a, _) => SOME a, fn () => NONE)

fun fromList xs =
   T {
      fold =
         fn (c, n) =>
            let
               fun go [] = n ()
                 | go (x :: xs) = c (x, fn () => go xs)
            in
               go xs
            end,
      size = Size.fromInt (List.length xs)
   }

fun fromListRev xs =
   T {
      fold =
         fn (c, n) =>
            let
               fun go ([], acc) = acc
                 | go (x :: xs, acc) = go (xs, c (x, fn () => acc))
            in
               go (xs, n ())
            end,
      size = Size.fromInt (List.length xs)
   }

fun fromPull s =
   T {
      fold =
         fn (c, n) =>
            let
               fun go s =
                  case Pull.uncons s of
                     NONE => n ()
                   | SOME (a, s') => c (a, fn () => go s')
            in
               go s
            end,
      size = Pull.size s
   }

datatype 'a stream = Nil | Cons of 'a * 'a stream thunk

fun toPull (T {fold, size}) =
   let
      fun step s =
         case s () of
            Nil => NONE
          | Cons (a, s) => SOME (a, s)
      fun seed () = fold (Cons, fn () => Nil)
   in
      Pull.unsafeSized (Pull.unfold step seed, size)
   end

fun countFrom i =
   T {
      fold = fn (c, n) =>
         let
            fun go i = c (i, fn () => go (i + 1))
         in
            go i
         end,
      size = Size.unknown
   }

fun countFrom' i =
   T {
      fold = fn (c, n) =>
         let
            val i = ref i
            fun go () =
               let
                  val x = !i
                  val () = i := !i + 1
               in
                  c (x, go)
               end
         in
            go ()
         end,
      size = Size.unknown
   }

fun map f (T {fold, size}) =
   T {
      fold = fn (c, n) => fold (fn (a, s) => c (f a, s), n),
      size = size
   }

fun concatMap f (T {fold, ...}) =
   T {
      fold =
         fn (c, n) =>
            let
               fun c' (a, s) =
                  case f a of
                     T {fold, ...} => fold (c, s)
            in
               fold (c', n)
            end,
      size = Size.unknown
   }

fun bind stream f = concatMap f stream

fun map2 f (stream1, stream2) =
   bind stream1 (fn a =>
   bind stream2 (fn b =>
   singleton (f (a, b))))

fun take i (T {fold, size}) =
   T {
      fold = fn (c, n) =>
         let
            fun c' (a, s) i =
               if i <= 0
                  then n ()
               else c (a, fn () => s () (i - 1))

            fun n' () _ = n ()
         in
            fold (c', n') i
         end,
      size = Size.min (Size.fromInt i, size)
   }

fun take' i (T {fold, size}) =
   T {
      fold = fn (c, n) =>
         let
            val i = ref i

            fun c' (a, s) =
               if !i <= 0
                  then n ()
               else (i := !i - 1; c (a, s))
         in
            fold (c', n)
         end,
      size = Size.min (Size.fromInt i, size)
   }

fun drop i (T {fold, size}) =
   T {
      fold =
         fn (c, n) =>
            let
               fun c' (a, s) i =
                  if i <= 0
                     then c (a, fn () => s () 0)
                  else s () (i - 1)

               fun n' () _ = n ()
            in
               fold (c', n') i
            end,
      size = Size.- (size, Size.fromInt i)
   }

fun filter p (T {fold, size}) =
   T {
      fold = fn (c, n) =>
         let
            fun c' (a, s) =
               if p a
                  then c (a, s)
               else s ()
         in
            fold (c', n)
         end,
      size = Size.toMax size
   }

fun append (T {fold = fold1, size = size1}, T {fold = fold2, size = size2}) =
   T {
      fold = fn (c, n) => fold1 (c, fn () => fold2 (c, n)),
      size = Size.+ (size1, size2)
   }

fun foldl f z (T {fold, ...}) =
   let
      fun c (a, s) b = s () (f (a, b))
      fun n () b = b
   in
      fold (c, n) z
   end

fun foldr f z (T {fold, ...}) =
   let
      fun c (a, s) = f (a, s ())
      fun n () = z
   in
      fold (c, n)
   end

fun toList s = foldr op :: [] s

fun toListRev s = foldl op :: [] s

fun size (T {size, ...}) = size

fun unsafeSized (T {fold, ...}, size) = T {fold = fold, size = size}

fun length (s as T {size, ...}) =
   case Size.exact size of
      SOME n => n
    | NONE => foldr (fn (_, n) => n + 1) 0 s

end

(* vim: set tw=0 ts=3 sw=3: *)
