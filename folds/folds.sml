
structure Folds =
struct
   structure U = Universal
   structure E = Either

   datatype ('a, 'b) t = Fold of {
      step: 'a * x -> x,
      init: x,
      done: x -> 'b
   }
   withtype x = U.universal

   val unitTag: unit U.tag = U.tag ()
   val unitUniversal = U.tagInject unitTag ()

   fun make step init done =
      let
         val t = U.tag ()
      in
         Fold {
            init = U.tagInject t init,
            step = fn (a, x) => U.tagInject t (step (a, U.tagProject t x)),
            done = fn x => done (U.tagProject t x)
         }
      end

   fun foldList (Fold {step, init, done}) xs =
      let
         (* Implement foldl as foldr for some reason *)
         fun f (a, k) x = k (step (a, x))
      in
         List.foldr f done xs init
      end

   fun map f (Fold {step, init, done}) =
      Fold {
         step = step,
         init = init,
         done = fn x => f (done x)
      }

   fun lmap f (Fold {step, init, done}) =
      Fold {
         step = fn (a, x) => step (f a, x),
         init = init,
         done = done
      }

   fun right (Fold {step, init, done}) =
      let
         val init' = E.INR init

         fun step' (E.INL c, _) = E.INL c
           | step' (_, E.INL c) = E.INL c
           | step' (E.INR a, E.INR x) = E.INR (step (a, x))

         val done' = E.mapRight done
      in
         make step' init' done'
      end

   fun left (Fold {step, init, done}) =
      let
         val init' = E.INL init

         fun step' (E.INR c, _) = E.INR c
           | step' (_, E.INR c) = E.INR c
           | step' (E.INL a, E.INL x) = E.INL (step (a, x))

         val done' = E.mapLeft done
      in
         make step' init' done'
      end

   fun pure x =
      Fold {
         init = unitUniversal,
         step = fn _ => unitUniversal,
         done = fn _ => x
      }

   fun ap (Fold {init = init1, step = step1, done = done1},
           Fold {init = init2, step = step2, done = done2}) =
      let
         val init = (init1, init2)
         fun done (x, y) = done1 x (done2 y)
         fun step (a, (x, y)) = (step1 (a, x), step2 (a, y))
      in
         make step init done
      end

   fun map2 f (Fold {init = init1, step = step1, done = done1},
               Fold {init = init2, step = step2, done = done2}) =
      let
         val init = (init1, init2)
         fun done (x, y) = f (done1 x, done2 y)
         fun step (a, (x, y)) = (step1 (a, x), step2 (a, y))
      in
         make step init done
      end

   fun extract (Fold {init, done, ...}) = done init

   fun duplicate (Fold {init, step, done}) =
      Fold {
         init = init,
         step = step,
         done = fn x => Fold {init = x, step = step, done = done}
      }

   fun extend x f = map f (duplicate x)

   fun op o (Fold {init = init1, step = step1, done = done1},
             Fold {init = init2, step = step2, done = done2}) =
      let
         val init = (init1, init2)

         fun step (a, (x, y)) =
            let
               val y' = step2 (a, y)
               val x' = step1 (done2 y', x)
            in
               (x', y')
            end

         fun done (x, _) = done1 x
      in
         make step init done
      end


   fun head () = make (fn (a, NONE) => SOME a | (_, SOME a) => SOME a) NONE Fn.id

   fun last () = make (fn (a, _) => SOME a) NONE Fn.id

   fun lastN n =
      let
         val init = Seq.empty
         val done = Seq.toList
         fun step (a, s) =
            if Seq.length s < n
               then Seq.snoc (s, a)
            else
               case Seq.viewl s of
                  NONE => Seq.singleton a
                | SOME (_, s') => Seq.snoc (Seq.Lazy.force s', a)
      in
         make step init done
      end

   local
      val t = U.tag ()
      val init = U.tagInject t false
      fun step _ = U.tagInject t true
      fun done x = U.tagProject t x
   in
      val null = Fold {init = init, step = step, done = done}
   end

   val sum = make Int.+ 0 Fn.id

   local
      val t = U.tag ()
      val init = U.tagInject t 0
      fun step (_, n) = U.tagInject t (U.tagProject t n + 1)
      val done = U.tagProject t
   in
      val length = Fold {
            init = init,
            step = step,
            done = done
         }
   end
end

structure Folds' : FOLDS = Folds
(* vim: set ft=sml ts=3 sw=3 tw=0: *)
