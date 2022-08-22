
structure Seq: SEQ =
struct

datatype 'a node =
   Nil
 | Cons of 'a * 'a t

withtype 'a t = unit -> 'a node

fun empty () = Nil

fun singleton x () = Cons (x, empty)

fun cons (x, xs) () = Cons (x, xs)

fun append (s1, s2) () =
   case s1 () of
      Nil => s2 ()
    | Cons (x, xs) => Cons (x, append (xs, s2))

fun map f s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) => Cons (f x, map f xs)

fun mapPartial f s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) =>
         case f x of
            NONE => mapPartial f xs ()
          | SOME y => Cons (y, mapPartial f xs)

fun filter f s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) =>
         if f x
            then Cons (x, filter f xs)
         else filter f xs ()

fun concat s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) => append (x, concat xs) ()

fun concatMap f s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) => append (f x, concatMap f xs) ()

fun foldl f z s =
   case s () of
      Nil => z
    | Cons (x, xs) => foldl f (f (x, z)) xs

fun app f s =
   case s () of
      Nil => ()
    | Cons (x, xs) => (f x; app f xs)

fun unfold f seed () =
   case f seed of
      NONE => Nil
    | SOME (x, seed') => Cons (x, unfold f seed')

fun isEmpty s =
   case s () of
      Nil => true
    | Cons _ => false

fun uncons s =
   case s () of
      Nil => NONE
    | Cons (x, xs) => SOME (x, xs)

fun length s =
   let
      fun go (s, acc) =
         case s () of
            Nil => acc
          | Cons (_, s') => go (s', acc + 1)
   in
      go (s, 0)
   end

fun appi f s =
   let
      fun go (i, s) =
         case s () of
            Nil => ()
          | Cons (x, xs) => (f (i, x); go (i + 1, xs))
   in
      go (0, s)
   end

fun foldli f z s =
   let
      fun go (i, s, acc) =
         case s () of
            Nil => acc
          | Cons (x, xs) => go (i + 1, xs, f (i, x, acc))
   in
      go (0, s, z)
   end

fun all p s =
   case s () of
      Nil => true
    | Cons (x, xs) => p x andalso all p xs

fun exists p s =
   case s () of
      Nil => false
    | Cons (x, xs) => p x orelse exists p xs

fun find p s =
   case s () of
      Nil => NONE
    | Cons (x, xs) => if p x then SOME x else find p xs

fun alli f s =
   let
      fun go (i, s) =
         case s () of
            Nil => true
          | Cons (x, xs) => f (i, x) andalso go (i + 1, xs)
   in
      go (0, s)
   end

fun existsi f s =
   let
      fun go (i, s) =
         case s () of
            Nil => false
          | Cons (x, xs) => f (i, x) orelse go (i + 1, xs)
   in
      go (0, s)
   end

fun tabulate (n, f) =
   let
      fun go i () =
         if i < n
            then Cons (f i, go (i + 1))
         else Nil
   in
      go 0
   end

fun repeat x () = Cons (x, repeat x)

fun forever f () = Cons (f (), forever f)

fun cycle s () =
   let
      fun go s () = append (s, go s) ()
   in
      case s () of
         Nil => Nil
       | Cons (x, xs) => Cons (x, append (xs, go s))
   end

fun iterate f x =
   let
      fun go x () =
         let
            val y = f x
         in
            Cons (y, go y)
         end
   in
      cons (x, go x)
   end

fun mapi f s =
   let
      fun go (i, s) () =
         case s () of
            Nil => Nil
          | Cons (x, xs) => Cons (f (i, x), go (i + 1, xs))
   in
      go (0, s)
   end

fun scan f seed s =
   let
      fun go (seed, s) () =
         case s () of
            Nil => Nil
          | Cons (x, xs) =>
               let
                  val seed' = f (x, seed)
               in
                  Cons (seed', go (seed', xs))
               end
   in
      cons (seed, go (seed, s))
   end

fun take n s =
   if n <= 0
      then empty
   else
      fn () =>
         case s () of
            Nil => Nil
          | Cons (x, xs) => Cons (x, take (n - 1) xs)

fun drop n s =
   if n <= 0
      then s
   else
      let
         fun go (n, s) =
            case s () of
               Nil => Nil
             | Cons (_, xs) =>
                  if n = 1
                     then xs ()
                  else go (n - 1, xs)
      in
         fn () => go (n, s)
      end

fun takeWhile p s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) => if p x then Cons (x, takeWhile p xs) else Nil

fun dropWhile p s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) => if p x then dropWhile p xs () else Cons (x, xs)

fun groupBy eq s () =
   case s () of
      Nil => Nil
    | Cons (x, xs) =>
         Cons (cons (x, takeWhile (fn y => eq (x, y)) xs),
               groupBy eq (dropWhile (fn y => eq (x, y)) xs))

structure Susp = SMLofNJ.Susp

fun memo f =
   let
      val susp = Susp.delay f
   in
      fn () => Susp.force susp
   end

fun memoize s =
   memo (fn () =>
      case s () of
         Nil => Nil
       | Cons (x, xs) => Cons (x, memoize xs))

fun zip (s1, s2) () =
   case s1 () of
      Nil => Nil
    | Cons (x, xs) =>
         case s2 () of
            Nil => Nil
          | Cons (y, ys) => Cons ((x, y), zip (xs, ys))

end

(* vim: set tw=0 ts=3 sw=3: *)
