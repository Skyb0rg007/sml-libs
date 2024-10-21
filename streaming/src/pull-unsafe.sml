
structure PullUnsafe :> STREAM =
struct

structure U = Unsafe

datatype 'a t = T of exn * (exn -> ('a * exn) option)

fun unfold next seed =
   T (U.cast seed, fn s => U.cast (next (U.cast s)))

val unfold: ('s -> ('a * 's) option) -> 's -> 'a t = unfold

fun fold f acc (T (seed, next)) =
   let
      fun loop (seed, acc) =
         case next seed of
            NONE => acc
          | SOME (x, seed') => loop (seed', f (x, acc))
   in
      loop (seed, acc)
   end

fun map f (T (seed, next)) =
   T (seed, fn s => Option.map (fn (x, s) => (f x, s)) (next s))

fun take n (T (seed, next)) =
   let
      fun next' (s, i) =
         if i <= 0
            then NONE
         else
            case next s of
               NONE => NONE
             | SOME (a, s') => SOME (a, (s', i - 1))
   in
      unfold next' (seed, n)
   end

fun filter p (T (seed, next)) =
   let
      fun next' s =
         case next s of
            NONE => NONE
          | SOME (a, s') => if p a then SOME (a, s') else next' s'
   in
      T (seed, next')
   end

fun concatMap f (T (seed, next)) =
   let
      fun next' {top, sub, sub_next} =
         case sub_next sub of
            SOME (x, sub') => SOME (x, {top = top, sub = sub', sub_next = sub_next})
          | NONE =>
               case next top of
                  NONE => NONE
                | SOME (x, top') =>
                     let
                        val T (sub, sub_next) = f x
                     in
                        next' {top = top', sub = sub, sub_next = sub_next}
                     end
   in
      unfold next' {top = seed, sub = U.cast (), sub_next = fn _ => NONE}
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
