
structure Test =
struct

infix 1 >>=

structure M =
   struct
      type 'a t = unit -> 'a
      fun pure x () = x
      fun map f x () = f (x ())
      fun (x >>= f) () = f (x ()) ()
   end

structure C = ConduitFn'(M)

structure P =
   struct
      fun unfold f seed =
         case f seed of
            NONE => C.pure ()
          | SOME (a, seed') => C.>>= (C.yield a, fn () => unfold f seed')

      fun fold f acc =
         C.>>= (C.await, fn NONE => C.pure acc
                          | SOME x => fold f (f (x, acc)))

      fun foldM f acc =
         C.>>= (C.await, fn NONE => C.pure acc
                          | SOME x => C.>>= (C.lift (f (x, acc)), foldM f))

      fun drop n =
         if n <= 0
            then C.pure ()
         else C.>>= (C.await, fn NONE => C.pure ()
                               | SOME _ => drop (n - 1))

      fun mapM_ f = C.awaitForever (fn i => C.lift (f i))

      fun take n =
         let
            fun go (front, n) =
               if n <= 0
                  then C.pure (front [])
               else C.>>= (C.await,
                  fn NONE => C.pure (front [])
                   | SOME x => go (fn xs => front (x :: xs), n - 1))
         in
            go (fn xs => xs, n)
         end

      fun peek () =
         C.>>= (C.await,
            fn NONE => C.pure NONE
             | SOME x => C.>>= (C.leftover x, fn () => C.pure (SOME x)))
   end

fun run () =
   let
      infix 1 |> >>=
      open C
      open P

      fun f 0 = NONE
        | f n = SOME (n, n - 1)
      
     val c = unfold f 10 |> take 4 |> mapM_ (fn x => fn () => print (x ^ "\n"))
   in
      run c ()
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
