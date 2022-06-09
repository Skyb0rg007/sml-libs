
structure Reducer
:> REDUCER
=
struct
   structure Either =
      struct
         open Either

         fun mapRight2 f (INL c, _) = INL c
           | mapRight2 f (_, INL c) = INL c
           | mapRight2 f (INR a, INR b) = INR (f (a, b))

         fun mapLeft2 f (INR c, _) = INR c
           | mapLeft2 f (_, INR c) = INR c
           | mapLeft2 f (INL a, INL b) = INL (f (a, b))
      end

   datatype 'a reduced =
      Reduced of 'a
    | Continue of 'a

   datatype ('s, 'a, 'b) t = T of {
      init: 's,
      step: 'a * 's -> 's reduced,
      done: 's -> 'b
   }

   fun reduceList (T {init, step, done}) xs =
      let
         fun go ([], acc) = done acc
           | go (x::xs, acc) =
            case step (x, acc) of
               Reduced acc' => done acc'
             | Continue acc' => go (xs, acc')
      in
         go (xs, init)
      end

   fun map f (T {init, step, done}) =
      T {
         init = init,
         step = step,
         done = fn x => f (done x)
      }

   fun lmap f (T {init, step, done}) =
      T {
         init = init,
         step = fn (a, s) => step (f a, s),
         done = done
      }

   fun left (T {init, step, done} : ('s, 'a, 'b) t) =
      T {
         init = Either.INL init,
         step = fn (Either.INR c, _) => Reduced (Either.INR c)
                 | (_, Either.INR c) => Reduced (Either.INR c)
                 | (Either.INL a, Either.INL s) =>
                  case step (a, s) of
                     Reduced x => Reduced (Either.INL x)
                   | Continue x => Continue (Either.INL x),
         done = Either.mapLeft done
      }

   fun right (T {init, step, done}) =
      T {
         init = Either.INR init,
         step = fn (Either.INL c, _) => Reduced (Either.INL c)
                 | (_, Either.INL c) => Reduced (Either.INL c)
                 | (Either.INR a, Either.INR s) =>
                  case step (a, s) of
                     Reduced x => Reduced (Either.INR x)
                   | Continue x => Continue (Either.INR x),
         done = Either.mapRight done
      }

   fun pure x =
      T {
         init = (),
         step = fn (_, ()) => Reduced (),
         done = fn () => x
      }

   local
      open Either

      fun map' _ (_, INL y) = Reduced y
        | map' f (x, INR y) = f (x, y)

      fun rproj (Continue x) = x
        | rproj (Reduced x) = x

      fun merge (Continue x, Continue y) = Continue (INR x, INR y)
        | merge (Reduced x, Continue y) = Continue (INL x, INR y)
        | merge (Continue x, Reduced y) = Continue (INR x, INL y)
        | merge (Reduced x, Reduced y) = Reduced (INL x, INL y)
   in
      type ('s, 't) ap_state = ('s, 's) either * ('t, 't) either

      fun ap (T {init = init1, step = step1, done = done1},
              T {init = init2, step = step2, done = done2}) =
         T {
            init = (INR init1, INR init2),
            step = fn (a, (s, t)) => merge (map' step1 (a, s), map' step2 (a, t)),
            done = fn (s, t) => done1 (proj s) (done2 (proj t))
         }

      fun map2 f (T {init = init1, step = step1, done = done1},
                  T {init = init2, step = step2, done = done2}) =
         T {
            init = (INR init1, INR init2),
            step = fn (a, (s, t)) => merge (map' step1 (a, s), map' step2 (a, t)),
            done = fn (s, t) => f (done1 (proj s), done2 (proj t))
         }

      type ('b, 's, 't) o_state = ('s, 's) either * ('b, 't) either

      fun op o (T {init = init1, step = step1, done = done1},
                T {init = init2, step = step2, done = done2}) =
         T {
            init = (INR init1, INR init2),
            step = fn (a, (INR s, INR t)) =>
                     let
                        val t' = step2 (a, t)
                        val b = done2 (rproj t')
                        val s' = step1 (b, s)
                     in
                        case (s', t') of
                           (Reduced x, _) => Reduced (INL x, INL b)
                         | (Continue x, Continue y) => Continue (INR x, INR y)
                         | (Continue x, Reduced y) => Continue (INR x, INL b)
                     end
                    | (_, (s as INL _, t)) => Reduced (s, t)
                    | (a, (INR s, INL b)) =>
                     case step1 (b, s) of
                        Reduced x => Reduced (INL x, INL b)
                      | Continue x => Continue (INR x, INL b),
            done = fn (s, _) => done1 (proj s)
         }
   end

   fun extract (T {init, done, ...}) = done init

   fun duplicate (T {init, step, done}) =
      T {
         init = init,
         step = step,
         done = fn x => T {
            init = x,
            step = step,
            done = done
         }
      }

   fun extend r f = map f (duplicate r)
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
