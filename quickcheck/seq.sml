
structure Seq: SEQ =
struct
   datatype 'a node = Nil | Cons of 'a * 'a t

   withtype 'a t = unit -> 'a node

   fun empty () = Nil

   fun cons (x, xs) () = Cons (x, xs)

   fun lcons (x, xs) () = Cons (x (), xs)

   fun uncons s =
      case s () of
         Nil => NONE
       | Cons (x, xs) => SOME (x, xs)

   fun map f s () =
      case s () of
         Nil => Nil
       | Cons (x, xs) => Cons (f x, map f xs)

   fun take (0, _) () = Nil
     | take (n, s) () =
      case s () of
         Nil => Nil
       | Cons (x, xs) => Cons (x, take (n - 1, xs))

   fun drop (0, s) () = s ()
     | drop (n, s) () =
      case s () of
         Nil => Nil
       | Cons (_, xs) => drop (n - 1, xs) ()

   fun fromList xs = List.foldr cons empty xs

   fun isEmpty s =
      case s () of
         Nil => true
       | Cons _ => false

   fun head s =
      case s () of
         Nil => NONE
       | Cons (x, _) => SOME x

   fun append (s1, s2) () =
      case s1 () of
         Nil => s2 ()
       | Cons (x, xs) => Cons (x, append (xs, s2))

   fun concatMap f s () =
      case s () of
         Nil => Nil
       | Cons (x, xs) => append (f x, concatMap f xs) ()

   fun concat s () =
      case s () of
         Nil => Nil
       | Cons (x, xs) => append (x, concat xs) ()

   fun filter p s () =
      case s () of
         Nil => Nil
       | Cons (x, xs) =>
            if p x
               then Cons (x, filter p xs)
            else filter p xs ()

   fun mapPartial f s () =
      case s () of
         Nil => Nil
       | Cons (x, xs) =>
            case f x of
               NONE => mapPartial f xs ()
             | SOME y => Cons (y, mapPartial f xs)

   fun ap (s1, s2) = concatMap (fn f => map f s2) s1

   fun map2 f (s1, s2) =
      concatMap
      (fn x => map (fn y => f (x, y)) s2)
      s1

   fun zipWith f (s1, s2) () =
      case (s1 (), s2 ()) of
         (Nil, _) => Nil
       | (_, Nil) => Nil
       | (Cons (x, xs), Cons (y, ys)) =>
            Cons (f (x, y), zipWith f (xs, ys))

   fun zip (s1, s2) = zipWith (fn p => p) (s1, s2)

   fun singleton x = cons (x, empty)

   fun repeat x () = Cons (x, repeat x)

   fun unfold f seed () =
      case f seed of
         NONE => Nil
       | SOME (x, seed') => Cons (x, unfold f seed')
end

(* vim: set tw=0 ts=3 sw=3: *)
