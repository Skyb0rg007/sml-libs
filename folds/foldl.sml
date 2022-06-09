
structure Foldl :> FOLDL =
struct

datatype ('s, 'a, 'b) t = T of {
   init: 's,
   step: 'a * 's -> ('b, 's) Either.either,
   done: 's -> 'b
}

type ('s, 't, 'a, 'b) map_state = ('a, 's) Either.either * ('b, 't) Either.either
type ('s, 't, 'b) o_state = 's * ('b, 't) Either.either
type 'a ord_set = 'a OrdSet.t

fun foldList (T {init, step, done}) xs =
   let
      fun go ([], s) = done s
        | go (x::xs, s) =
         case step (x, s) of
            Either.INL b => b
          | Either.INR s' => go (xs, s')
   in
      go (xs, init)
   end

fun foldOption (T {init, done, ...}) NONE = done init
  | foldOption (T {init, step, done}) (SOME a) =
   case step (a, init) of
      Either.INL b => b
    | Either.INR s => done s

fun foldVector (T {init, step, done}) v =
   let
      val len = Vector.length v
      fun go (i, s) =
         if i >= len
            then done s
         else
            case step (Vector.sub (v, i), s) of
               Either.INL b => b
             | Either.INR s' => go (i + 1, s')
   in
      go (0, init)
   end

fun foldArray (T {init, step, done}) arr =
   let
      val len = Array.length arr
      fun go (i, s) =
         if i >= len
            then done s
         else
            case step (Array.sub (arr, i), s) of
               Either.INL b => b
             | Either.INR s' => go (i + 1, s')
   in
      go (0, init)
   end

fun foldGetc (T {init, step, done}) getc =
   let
      fun go s =
         case getc () of
            NONE => done s
          | SOME c =>
               case step (c, s) of
                  Either.INL b => b
                | Either.INR s' => go s'
   in
      go init
   end

fun foldTextIO f strm = foldGetc f (fn () => TextIO.input1 strm)

fun foldTextIOLine f strm = foldGetc f (fn () => TextIO.inputLine strm)

fun foldBinIO f strm = foldGetc f (fn () => BinIO.input1 strm)

(* Combinators *)

fun map f (T {init, step, done}) =
   T {
      init = init,
      step = fn (a, s) =>
         case step (a, s) of
            Either.INL b => Either.INL (f b)
          | Either.INR s' => Either.INR s',
      done = fn s => f (done s)
   }

fun lmap f (T {init, step, done}) =
   T {
      init = init,
      step = fn (a, s) => step (f a, s),
      done = done
   }

fun dimap g f (T {init, step, done}) =
   T {
      init = init,
      step = fn (a, s) =>
         case step (g a, s) of
            Either.INL b => Either.INL (f b)
          | Either.INR s' => Either.INR s',
      done = fn s => f (done s)
   }

fun left (T {init, step, done}) =
   T {
      init = init,
      step = fn (Either.INR c, _) => Either.INL (Either.INR c)
              | (Either.INL a, s) =>
                  case step (a, s) of
                     Either.INL b => Either.INL (Either.INL b)
                   | Either.INR s' => Either.INR s',
      done = fn s => Either.INL (done s)
   }

fun right (T {init, step, done}) =
   T {
      init = init,
      step = fn (Either.INL c, _) => Either.INL (Either.INL c)
              | (Either.INR a, s) =>
                  case step (a, s) of
                     Either.INL b => Either.INL (Either.INR b)
                   | Either.INR s' => Either.INR s',
      done = fn s => Either.INR (done s)
   }

fun pure b = T {
      init = (),
      step = fn (_, ()) => Either.INL b,
      done = fn () => b
   }

fun map2 f (T {init = init1, step = step1, done = done1},
            T {init = init2, step = step2, done = done2}) =
   let
      fun step' _ (_, Either.INL b) = Either.INL b
        | step' step (a, Either.INR s) = step (a, s)

      fun done' _ (Either.INL b) = b
        | done' done (Either.INR s) = done s
   in
      T {
         init = (Either.INR init1, Either.INR init2),
         step = fn (a, (s, t)) =>
            case (step' step1 (a, s), step' step2 (a, t)) of
               (Either.INL b, Either.INL c) => Either.INL (f (b, c))
             | (x, y) => Either.INR (x, y),
         done = fn (s, t) => f (done' done1 s, done' done2 t)
      }
   end

fun ap (f, x) = map2 (fn (f, x) => f x) (f, x)

fun extract (T {init, done, ...}) = done init

fun duplicate (T {init, step, done}) =
   T {
      init = init,
      step = fn (a, s) =>
         case step (a, s) of
            Either.INL b => Either.INL (T {init = init, step = fn _ => Either.INL b, done = fn _ => b})
          | Either.INR s' => Either.INR s',
      done = fn s => T {init = s, step = step, done = done}
   }

fun extend r f = map f (duplicate r)

fun op o (T {init = init1, step = step1, done = done1} : ('s, 'b, 'c) t,
          T {init = init2, step = step2, done = done2} : ('t, 'a, 'b) t)
          : (('s, 't, 'b) o_state, 'a, 'c) t =
   let
      fun done' _ (Either.INL b) = b
        | done' done (Either.INR s) = done s
   in
      T {
         init = (init1, Either.INR init2),
         step = fn (a, (s, Either.INR t)) =>
                  let
                     val t' = step2 (a, t)
                     val b = done' done2 t'
                  in
                     case step1 (b, s) of
                        Either.INL c => Either.INL c
                      | Either.INR s' => Either.INR (s', t')
                  end
                 | (a, (s, Either.INL b)) =>
                  case step1 (b, s) of
                     Either.INL c => Either.INL c
                   | Either.INR s' => Either.INR (s', Either.INL b),
         done = fn (s, _) => done1 s
      }
   end

val length = T {
   init = 0,
   step = fn (_, n) => Either.INR (n + 1),
   done = fn n => n
}

val sum = T {
   init = 0,
   step = fn (a, n) => Either.INR (a + n),
   done = fn n => n
}

val product = T {
   init = 1,
   step = fn (a, n) =>
      case a * n of
         0 => Either.INL 0
       | m => Either.INR m,
   done = fn n => n
}

val head = T {
   init = (),
   step = fn (a, ()) => Either.INL (SOME a),
   done = fn () => NONE
}

val last = T {
   init = NONE,
   step = fn (a, _) => Either.INR (SOME a),
   done = fn x => x
}

val null = T {
   init = (),
   step = fn (_, ()) => Either.INL false,
   done = fn () => true
}

fun all p = T {
      init = (),
      step = fn (a, ()) =>
         if p a
            then Either.INR ()
         else Either.INL false,
      done = fn () => true
   }

fun any p = T {
      init = (),
      step = fn (a, ()) =>
         if p a
            then Either.INL true
         else Either.INR (),
      done = fn () => false
   }

val list = T {
   init = fn x => x,
   step = fn (a, s) => Either.INR (fn x => s (a :: x)),
   done = fn s => s []
}

val revList = T {
   init = [],
   step = fn (a, s) => Either.INR (a :: s),
   done = fn s => s
}

fun reduce f = T {
      init = NONE,
      step = fn (a, NONE) => Either.INR (SOME a)
              | (a, SOME s) => Either.INR (SOME (f (a, s))),
      done = fn s => s
   }

fun filter p (T {init, step, done}) = T {
      init = init,
      step = fn (a, s) =>
         if p a
            then step (a, s)
         else Either.INR s,
      done = done
   }

fun dropWhile p (T {init, step, done}) = T {
      init = (true, init),
      step = fn (a, (dropping, s)) =>
         if dropping andalso p a
            then Either.INR (true, s)
         else
            case step (a, s) of
               Either.INL b => Either.INL b
             | Either.INR s' => Either.INR (false, s'),
      done = fn (_, s) => done s
   }

fun drop n (T {init, step, done}) = T {
      init = (n, init),
      step = fn (a, (n, s)) =>
         if n <= 0
            then
               case step (a, s) of
                  Either.INL b => Either.INL b
                | Either.INR s' => Either.INR (0, s')
         else Either.INR (n - 1, s),
      done = fn (_, s) => done s
   }

fun either (T {init = init1, step = step1, done = done1},
            T {init = init2, step = step2, done = done2}) =
   let
      fun step1' (_, Either.INL b1) = Either.INL b1
        | step1' (Either.INR _, Either.INR s) = Either.INR s
        | step1' (Either.INL a1, Either.INR s) = step1 (a1, s)

      fun step2' (_, Either.INL b2) = Either.INL b2
        | step2' (Either.INL _, Either.INR s) = Either.INR s
        | step2' (Either.INR a2, Either.INR s) = step2 (a2, s)

      fun done' _ (Either.INL b) = b
        | done' done (Either.INR s) = done s
   in
      T {
         init = (Either.INR init1, Either.INR init2),
         step = fn (a, (s, t)) =>
            case (step1' (a, s), step2' (a, t)) of
               (Either.INL b, Either.INL c) => Either.INL (b, c)
             | (x, y) => Either.INR (x, y),
         done = fn (s, t) => (done' done1 s, done' done2 t)
      }
   end

fun eqNub eq = T {
      init = ([], fn x => x),
      step = fn (a, (known, s)) =>
         if List.exists (fn b => eq (a, b)) known
            then Either.INR (known, s)
         else Either.INR (a :: known, fn x => s (a :: x)),
      done = fn (_, s) => s []
   }

fun nub cmp = T {
      init = (OrdSet.empty cmp, fn x => x),
      step = fn (a, (known, s)) =>
         if OrdSet.member (known, a)
            then Either.INR (known, s)
         else Either.INR (OrdSet.insert (known, a), fn x => s (a :: x)),
      done = fn (_, s) => s []
   }

end
(* vim: set ft=sml tw=0 sw=3 ts=3: *)
