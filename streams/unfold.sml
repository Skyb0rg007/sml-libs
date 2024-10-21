
structure Unfold =
struct

datatype 'b step =
    Yield of 'b * exn
  | Skip of exn
  | Stop

datatype ('a, 'b) t = Unfold of {
    inject : 'a -> exn,
    step : exn -> 'b step
  }

exception Impossible

fun mapStep f (Yield (x, s)) = Yield (f x, s)
  | mapStep _ (Skip s) = Skip s
  | mapStep _ Stop = Stop

fun map f (Unfold {inject, step}) =
      Unfold {inject=inject, step=fn s => mapStep f (step s)}

fun premap f (Unfold {inject, step}) =
      Unfold {inject=fn a => inject (f a), step=step}

fun unfoldr step = let
      exception S of 's
      fun step' (S s) =
            (case step s
              of NONE => Stop
               | SOME (x, s) => Yield (x, S s))
        | step' _ = raise Impossible
      in
        Unfold {inject=S, step=step'}
      end

fun takeWhileWithInput p (Unfold {step, inject}) = let
      exception S of 'a * exn
      fun inject' a = S (a, inject a)
      fun step' (S (a, s)) =
            (case step s
              of Yield (b, s) => if p (a, b) then Yield (b, S (a, s)) else Stop
               | Skip s => Skip (S (a, s))
               | Stop => Stop)
        | step' _ = raise Impossible
      in
        Unfold {step=step', inject=inject'}
      end

fun takeWhile p (Unfold {step, inject}) = let
      fun step' s =
            case step s
              of res as Yield (b, _) => if p b then res else Stop
               | Skip s => Skip s
               | Stop => Stop
      in
        Unfold {step=step', inject=inject}
      end

fun concatMap f (Unfold {step, inject}) = let
      exception Outer of 'a * exn
      exception Inner of 'a * exn * exn * (exn -> 'b step)
      fun inject' a = Outer (a, inject a)
      fun step' (Outer (a, s)) =
            (case step s
              of Yield (x, s) => let
                   val Unfold {step=stepInner, inject=injectInner} = f x
                   in
                     Skip (Inner (a, s, injectInner a, stepInner))
                   end
               | Skip s => Skip (Outer (a, s))
               | Stop => Stop)
        | step' (Inner (a, s, sInner, stepInner)) =
            (case stepInner sInner
              of Yield (x, sInner') => Yield (x, Inner (a, s, sInner', stepInner))
               | Skip sInner' => Skip (Inner (a, s, sInner', stepInner))
               | Stop => Skip (Outer (a, s)))
        | step' _ = raise Impossible
      in
        Unfold {step=step', inject=inject'}
      end

fun fromFunction f = let
      exception S of 'a
      fun inject x = S x
      fun step (S x) = Yield (f x, Empty)
        | step _ = Stop
      in
        Unfold {step=step, inject=inject}
      end

fun zipWith f (Unfold {step=step1, inject=inject1}, Unfold {step=step2, inject=inject2}) = let
      exception S of exn * exn * 'b option
      fun inject x = S (inject1 x, inject2 x, NONE)
      fun step (S (s1, s2, NONE)) =
            (case step1 s1
              of Yield (x, s) => Skip (S (s, s2, SOME x))
               | Skip s => Skip (S (s, s2, NONE))
               | Stop => Stop)
        | step (S (s1, s2, SOME x)) =
            (case step2 s2
              of Yield (y, s) => Yield (f (x, y), S (s1, s, NONE))
               | Skip s => Skip (S (s1, s, SOME x))
               | Stop => Stop)
        | step _ = raise Impossible
      in
        Unfold {step=step, inject=inject}
      end

local
  exception Outer of exn * exn list
  exception Inner of exn * exn list
  exception InnerL of exn list * exn list
  exception InnerR of exn list * exn list
  nonfix o
in
fun manyInterleave (Unfold {step=istep, inject=iinject}, Unfold {step=ostep, inject=oinject}) = let
      fun inject x = Outer (oinject x, [])
      fun step (Outer (o, ls)) =
            (case ostep o
              of Yield (a, o') => Skip (Inner (o', iinject a :: ls))
               | Skip o' => Skip (Outer (o', ls))
               | Stop => Skip (InnerL (ls, [])))
        | step (Inner (o, st :: ls)) =
            (case istep st
              of Yield (x, s) => Yield (x, Outer (o, s :: ls))
               | Skip s => Skip (Inner (o, s :: ls))
               | Stop => Skip (Outer (o, ls)))
        | step (InnerL ([], [])) = Stop
        | step (InnerL ([], rs)) = Skip (InnerR ([], rs))
        | step (InnerL (st :: ls, rs)) =
            (case istep st
              of Yield (x, s) => Yield (x, InnerL (ls, s :: rs))
               | Skip s => Skip (InnerL (s :: ls, rs))
               | Stop => Skip (InnerL (ls, rs)))
        | step (InnerR ([], [])) = Stop
        | step (InnerR (ls, [])) = Skip (InnerL (ls, []))
        | step (InnerR (ls, st :: rs)) =
            (case istep st
              of Yield (x, s) => Yield (x, InnerR (s :: ls, rs))
               | Skip s => Skip (InnerR (ls, s :: rs))
               | Stop => Skip (InnerR (ls, rs)))
        | step _ = raise Impossible
      in
        Unfold {step=step, inject=inject}
      end
end

end
