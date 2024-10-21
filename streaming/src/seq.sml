
structure Seq :> STREAM =
struct
   datatype 'a node = Nil | Cons of 'a * 'a t

   withtype 'a t = unit -> 'a node

   fun empty () = Nil

   fun map f seq () =
      case seq () of
         Nil => Nil
       | Cons (x, xs) => Cons (f x, map f xs)

   fun filter p seq () =
      case seq () of
         Nil => Nil
       | Cons (x, xs) =>
            if p x
               then Cons (x, filter p xs)
               else filter p xs ()

   fun concatMap f seq () =
      case seq () of
         Nil => Nil
       | Cons (x, xs) => concatMap' f (f x) xs ()

   and concatMap' f seq tail () =
      case seq () of
         Nil => concatMap f tail ()
       | Cons (x, xs) => Cons (x, concatMap' f xs tail)

   fun fold f acc seq =
      case seq () of
         Nil => acc
       | Cons (x, xs) => fold f (f (x, acc)) xs

   fun take n seq () =
      case seq () of
         Nil => Nil
       | Cons (x, xs) => if n = 0 then Nil else Cons (x, take (n - 1) xs)

   fun unfold f seed () =
      case f seed of
         NONE => Nil
       | SOME (x, seed') => Cons (x, unfold f seed')
end

(* vim: set tw=0 ts=3 sw=3: *)
