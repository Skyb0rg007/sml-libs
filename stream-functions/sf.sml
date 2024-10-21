
structure SF: SF =
struct

infixr 1 >>> <<< >>^ ^>> <<^ ^<<
infixr 2 +++ |||
infixr 3 *** &&&

type dtime = real

datatype ('a, 'b) transition = T of ('a, 'b) t * 'b

withtype ('a, 'b) t = dtime * 'a -> ('a, 'b) transition

fun identity (_, a) = T (identity, a)

fun constant a (_, _) = T (constant a, a)

fun arr f (_, a) = T (arr f, f a)

fun swap (_, (a, b)) = T (swap, (b, a))

fun dup (_, a) = T (dup, (a, a))

fun (f >>> g) (dt, a) =
   let
      val T (f', b) = f (dt, a)
      val T (g', c) = g (dt, b)
   in
      T (f' >>> g', c)
   end

fun (f >>^ g) (dt, a) =
   let
      val T (f', b) = f (dt, a)
   in
      T (f' >>^ g, g b)
   end

fun (f ^>> g) (dt, a) =
   let
      val T (g', c) = g (dt, f a)
   in
      T (f ^>> g', c)
   end

fun g <<< f = f >>> g
fun g ^<< f = f >>^ g
fun g <<^ f = f ^>> g

fun first f (dt, (a, c)) =
   let
      val T (f', b) = f (dt, a)
   in
      T (first f', (b, c))
   end

fun second f (dt, (c, a)) =
   let
      val T (f', b) = f (dt, a)
   in
      T (second f', (c, b))
   end

fun (f *** g) (dt, (a, c)) =
   let
      val T (f', b) = f (dt, a)
      val T (g', d) = g (dt, c)
   in
      T (f' *** g', (b, d))
   end

fun (f &&& g) (dt, a) =
   let
      val T (f', b) = f (dt, a)
      val T (g', c) = g (dt, a)
   in
      T (f' &&& g', (b, c))
   end

fun left f (dt, Either.INL a) =
   let
      val T (f', b) = f (dt, a)
   in
      T (left f', Either.INL b)
   end
  | left f (_, Either.INR c) = T (left f, Either.INR c)

fun right f (dt, Either.INR a) =
   let
      val T (f', b) = f (dt, a)
   in
      T (right f', Either.INR b)
   end
  | right f (_, Either.INL c) = T (right f, Either.INL c)

fun (f +++ g) (dt, Either.INL a) =
   let
      val T (f', b) = f (dt, a)
   in
      T (f' +++ g, Either.INL b)
   end
  | (f +++ g) (dt, Either.INR c) =
   let
      val T (g', d) = g (dt, c)
   in
      T (f +++ g', Either.INR d)
   end

fun (f ||| g) (dt, Either.INL a) =
   let
      val T (f', c) = f (dt, a)
   in
      T (f' ||| g, c)
   end
  | (f ||| g) (dt, Either.INR b) =
   let
      val T (g', c) = g (dt, b)
   in
      T (f ||| g', c)
   end

fun feedback c f (dt, a) =
   let
      val T (f', (b, c')) = f (dt, (a, c))
   in
      T (feedback c' f', b)
   end

fun scan f z =
   let
      fun g (a, s) =
         let
            val s' = f (a, s)
         in
            (s', s')
         end
   in
      feedback z (arr g)
   end

fun mapAccum f z = feedback z (arr f)

fun unfold f z = feedback z (arr (fn (_, b) => f b))

fun pre a = feedback a swap

fun post b f =
   f >>> feedback (SOME b)
      (arr (fn (c, NONE) => (c, NONE) | (_, SOME b') => (b', NONE)))

fun next b f = f >>> pre b

fun fifo args =
   let
      fun g (xs, q) =
         case q @ xs of
            [] => (NONE, [])
          | x::xs => (SOME x, xs)
   in
      feedback [] (arr g) args
   end

fun switch f (dt, a) =
   case f (dt, a) of
      T (f', Either.INR b) => T (switch f', b)
    | T (_, Either.INL f') => f' (0.0, a)

local
   fun localTime' time (dt, ()) =
      let
         val time' = time + dt
      in
         T (localTime' time', time')
      end
in
   val localTime = localTime' 0.0
end

val integral =
   let
      fun loop (dt, (x, area)) =
         let
            val area' = dt * x + area
         in
            T (loop, (area', area'))
         end
   in
      feedback 0.0 loop
   end

fun reactimate {initial, input, output} f =
   let
      fun loop (f, a, b) =
         if output b
            then ()
         else
            let
               val (dt, opt) = input ()
               val a' = Option.getOpt (opt, a)
               val T (f', b') = f (dt, a')
            in
               loop (f', a', b')
            end

      val T (f', b0) = f (0.0, initial)
   in
      loop (f', initial, b0)
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
