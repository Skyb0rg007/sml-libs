
structure Fold : FOLD =
struct

datatype 'b step =
    Partial of exn
  | Done of 'b

datatype ('a, 'b) t = Fold of {
    init : unit -> 'b step,
    step : 'a * exn -> 'b step,
    extract : exn -> 'b,
    final : exn -> 'b
  }

exception Impossible
exception Unsupported

fun foldl f ini = let
  exception S of 'b
  fun get (S s) = s
    | get _ = raise Impossible
  in
    Fold {
      init = fn () => Partial (S ini),
      step = fn (a, acc) => Partial (S (f (a, get acc))),
      extract = get,
      final = get
    }
  end

fun foldl1 f = let
  exception None
  exception Some of 'a
  fun toOption None = NONE
    | toOption (Some x) = SOME x
    | toOption _ = raise Impossible
  in
    Fold {
      init = fn () => Partial None,
      step = fn (a, None) => Partial (Some a)
              | (a, Some acc) => Partial (Some (f (a, acc)))
              | _ => raise Impossible,
      extract = toOption,
      final = toOption
    }
  end

fun mapStep f (Done b) = Done (f b)
  | mapStep f (Partial s) = Partial s

fun map f (Fold {init, step, extract, final}) =
      Fold {
        init = fn () => mapStep f (init ()),
        step = fn (a, s) => mapStep f (step (a, s)),
        extract = fn s => f (extract s),
        final = fn s => f (final s)
      }

fun foldr f ini =
      map (fn k => k ini)
        (foldl (fn (x, k) => fn acc => k (f (x, acc))) Fn.id)


fun pure b =
      Fold {
        init = fn () => Done b,
        step = fn _ => raise Impossible,
        extract = fn _ => raise Impossible,
        final = fn _ => raise Impossible
      }

fun splitWith f (fold1, fold2) = let
      val Fold {init=init1, step=step1, extract=extract1, final=final1} = fold1
      val Fold {init=init2, step=step2, extract=extract2, final=final2} = fold2
      exception SeqFold1 of exn
      exception SeqFold2 of exn * 'b
      fun run2 (Done c, b) = Done (f (b, c))
        | run2 (Partial s2, b) = Partial (SeqFold2 (s2, b))
      fun run1 (Partial s1) = Partial (SeqFold1 s1)
        | run1 (Done b) = run2 (init2 (), b)
      fun init () = run1 (init1 ())
      fun step (a, SeqFold1 s1) = run1 (step1 (a, s1))
        | step (a, SeqFold2 (s2, b)) = run2 (step2 (a, s2), b)
        | step _ = raise Impossible
      fun final (SeqFold2 (s2, b)) = f (b, final2 s2)
        | final (SeqFold1 s1) = let
            val b = final1 s1
            in
              case init2 ()
                of Done c => f (b, c)
                 | Partial s2 => f (b, final2 s2)
            end
        | final _ = raise Impossible
      in
        Fold {
          init = init,
          step = step,
          extract = fn _ => raise Unsupported,
          final = final
        }
      end

fun teeWith f (fold1, fold2) = let
      val Fold {init=init1, step=step1, extract=extract1, final=final1} = fold1
      val Fold {init=init2, step=step2, extract=extract2, final=final2} = fold2
      exception Left of 'b * exn
      exception Right of exn * 'c
      exception Both of exn * exn
      fun run (Partial s1, Partial s2) = Partial (Both (s1, s2))
        | run (Partial s1, Done c) = Partial (Right (s1, c))
        | run (Done b, Partial s2) = Partial (Left (b, s2))
        | run (Done b, Done c) = Done (f (b, c))
      fun step (a, Both (s1, s2)) = run (step1 (a, s1), step2 (a, s2))
        | step (a, Left (b, s2)) = run (Done b, step2 (a, s2))
        | step (a, Right (s1, c)) = run (step1 (a, s1), Done c)
        | step _ = raise Impossible
      fun extract (Both (s1, s2)) = f (extract1 s1, extract2 s2)
        | extract (Left (b, s2)) = f (b, extract2 s2)
        | extract (Right (s1, c)) = f (extract1 s1, c)
        | extract _ = raise Impossible
      fun final (Both (s1, s2)) = f (final1 s1, final2 s2)
        | final (Left (b, s2)) = f (b, final2 s2)
        | final (Right (s1, c)) = f (final1 s1, c)
        | final _ = raise Impossible
      in
        Fold {
          init = fn () => run (init1 (), init2 ()),
          step = step,
          extract = extract,
          final = final
        }
      end

fun teeWithFst f (fold1, fold2) = let
      val Fold {init=init1, step=step1, extract=extract1, final=final1} = fold1
      val Fold {init=init2, step=step2, extract=extract2, final=final2} = fold2
      exception Right of exn * 'c
      exception Both of exn * exn
      fun run (Partial s1, Partial s2) = Partial (Both (s1, s2))
        | run (Partial s1, Done c) = Partial (Right (s1, c))
        | run (Done b, Partial s2) = Done (f (b, final2 s2))
        | run (Done b, Done c) = Done (f (b, c))
      fun step (a, Both (s1, s2)) = run (step1 (a, s1), step2 (a, s2))
        | step (a, Right (s1, c)) = run (step1 (a, s1), Done c)
        | step _ = raise Impossible
      fun extract (Both (s1, s2)) = f (extract1 s1, extract2 s2)
        | extract (Right (s1, c)) = f (extract1 s1, c)
        | extract _ = raise Impossible
      fun final (Both (s1, s2)) = f (final1 s1, final2 s2)
        | final (Right (s1, c)) = f (final1 s1, c)
        | final _ = raise Impossible
      in
        Fold {
          init = fn () => run (init1 (), init2 ()),
          step = step,
          extract = extract,
          final = final
        }
      end

fun teeWithMin f (fold1, fold2) = let
      val Fold {init=init1, step=step1, extract=extract1, final=final1} = fold1
      val Fold {init=init2, step=step2, extract=extract2, final=final2} = fold2
      exception Both of exn * exn
      fun run (Partial s1, Partial s2) = Partial (Both (s1, s2))
        | run (Partial s1, Done c) = Done (f (final1 s1, c))
        | run (Done b, Partial s2) = Done (f (b, final2 s2))
        | run (Done b, Done c) = Done (f (b, c))
      fun step (a, Both (s1, s2)) = run (step1 (a, s1), step2 (a, s2))
        | step _ = raise Impossible
      fun extract (Both (s1, s2)) = f (extract1 s1, extract2 s2)
        | extract _ = raise Impossible
      fun final (Both (s1, s2)) = f (final1 s1, final2 s2)
        | final _ = raise Impossible
      in
        Fold {
          init = fn () => run (init1 (), init2 ()),
          step = step,
          extract = extract,
          final = final
        }
      end

fun concatMap f (Fold {init, step, extract, final}) = let
      exception B of exn * (exn -> 'b)
      exception C of exn * ('a * exn -> 'c step) * (exn -> 'c) * (exn -> 'c)
      fun initInner (Fold {init=i, step=st, extract=e, final=fin}) =
        case i ()
          of Partial s => Partial (C (s, st, e, fin))
           | res as Done _ => res
      fun init' () =
        case init ()
          of Partial s => Partial (B (s, final))
           | Done b => initInner (f b)
      fun step' (a, B (s, fin)) =
        (case step (a, s)
          of Partial s => Partial (B (s, fin))
           | Done b => initInner (f b))
        | step' (a, C (s, st, e, fin)) =
        (case st (a, s)
          of Partial s => Partial (C (s, st, e, fin))
           | res as Done _ => res)
        | step' _ = raise Impossible
      fun final' (C (s, _, _, fin)) = fin s
        | final' (B (s, fin)) = let
            val Fold {init=i, final=fin, ...} = f (fin s)
            in
              case i ()
                of Partial s => fin s
                 | Done c => c
            end
        | final' _ = raise Impossible
      in
        Fold {
          init = init',
          step = step',
          extract = fn _ => raise Unsupported,
          final = final'
        }
      end

fun runList (Fold {init, step, final, ...}, lst) =
  case init ()
    of Done b => b
     | Partial s => let
         fun go (s, []) = final s
           | go (s, x :: xs) =
               case step (x, s)
                 of Done b => b
                  | Partial s => go (s, xs)
          in
            go (s, lst)
          end

fun driveWith unfold seed (Fold {init, step, final, ...}) =
  case init ()
    of Done b => b
     | Partial s => let
         fun go (s, seed) =
           case unfold seed
             of NONE => final s
              | SOME (x, seed') =>
                  case step (x, s)
                    of Done b => b
                     | Partial s' => go (s', seed')
         in
           go (s, seed)
         end

fun drive (Unfold.Unfold {step=ustep, inject}) (Fold {init, step, final, ...}) a =
  case init ()
    of Done b => b
     | Partial s => let
         fun go (s, seed) =
           case ustep seed
             of Unfold.Stop => final s
              | Unfold.Skip seed' => go (s, seed')
              | Unfold.Yield (x, seed') =>
                  case step (x, s)
                    of Done b => b
                     | Partial s' => go (s', seed')
         in
           go (s, inject a)
         end

end
