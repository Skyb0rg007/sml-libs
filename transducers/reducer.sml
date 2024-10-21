
structure Reducer : REDUCER =
struct
  structure Step =
    struct
      datatype ('s, 'b) t = More of 's | Done of 'b

      fun lmap f (More s) = More (f s)
        | lmap _ (Done b) = Done b

      fun rmap _ (More s) = More s
        | rmap g (Done b) = Done (g b)
    end

  structure F =
    struct
      type 'a t = 'a list

      val map = List.map
    end

  datatype ('s, 'a, 'b) t' = R of {
    init : unit -> ('s, 'b) Step.t,
    step : 'a * 's -> ('s, 'b) Step.t,
    done : 's -> 'b
  }

  (* Treat this as ∃'s. ('s, 'a, 'b) t' *)
  type ('a, 'b) t = (exn, 'a, 'b) t'

  exception Impossible

  fun lmap f (R {init, step, done}) =
    R {init = init, step = fn (x, s) => step (f x, s), done = done}

  fun rmap g (R {init, step, done}) =
    R {init = Step.rmap g o init, step = Step.rmap g o step, done = g o done}

  fun dimap f g (R {init, step, done}) =
    R {init = Step.rmap g o init,
       step = fn (x, s) => Step.rmap g (step (f x, s)),
       done = g o done}

  fun cosieve (R {init, step, done}) lst =
    let
      fun loop ([], s) = done s
        | loop (x :: xs, s) =
          case step (x, s) of
              Step.Done b => b
            | Step.More s' => loop (xs, s')
    in
      case init () of
          Step.Done b => b
        | Step.More s => loop (lst, s)
    end

  fun cotabulate f =
    let
      exception State of 'a list

      fun init () = Step.More (State [])

      fun step (x, State lst) = Step.More (State (x :: lst))
        | step _ = raise Impossible

      fun done (State lst) = f lst
        | done _ = raise Impossible
    in
      R {init = init, step = step, done = done}
    end

  fun pure b =
    R {init = fn () => Step.Done b,
       step = fn _ => raise Fail "Reducer.pure: step function was called",
       done = fn _ => raise Fail "Reducer.pure: done function was called"}

  fun map2 f (r1, r2) =
    let
      val R {init = init1, step = step1, done = done1} = r1
      val R {init = init2, step = step2, done = done2} = r2

      exception NotDone of 's1 * 's2
      exception Done1 of 'b * 's2
      exception Done2 of 's1 * 'c

      val merge =
        fn (Step.Done b, Step.Done c) => Step.Done (f (b, c))
         | (Step.Done b, Step.More s2) => Step.More (Done1 (b, s2))
         | (Step.More s1, Step.Done c) => Step.More (Done2 (s1, c))
         | (Step.More s1, Step.More s2) => Step.More (NotDone (s1, s2))

      fun init () = merge (init1 (), init2 ())

      fun done (NotDone (s1, s2)) = f (done1 s1, done2 s2)
        | done (Done1 (b, s2)) = f (b, done2 s2)
        | done (Done2 (s1, c)) = f (done1 s1, c)
        | done _ = raise Impossible

      fun step (x, NotDone (s1, s2)) = merge (step1 (x, s1), step2 (x, s2))
        | step (x, Done1 (b, s2)) = merge (Step.Done b, step2 (x, s2))
        | step (x, Done2 (s1, c)) = merge (step1 (x, s1), Step.Done c)
        | step _ = raise Impossible
    in
      R {init = init, step = step, done = done}
    end

  fun both (r1, r2) = map2 (fn (a, b) => (a, b)) (r1, r2)

  fun prefilter p (R {init, step, done}) =
    let
      fun step' (x, s) = if p x then step (x, s) else Step.More s
    in
      R {init = init, step = step', done = done}
    end

  fun premapPartial f (R {init, step, done}) =
    let
      fun step' (x, s) =
        case f x of
            NONE => Step.More s
          | SOME y => step (y, s)
    in
      R {init = init, step = step', done = done}
    end

  fun left (R {init, step, done}) =
    let
      fun init' () = Step.rmap Either.INL (init ())

      fun step' (Either.INL x, s) = Step.rmap Either.INL (step (x, s))
        | step' (Either.INR x, _) = Step.Done (Either.INR x)

      fun done' s = Either.INL (done s)
    in
      R {init = init', step = step', done = done'}
    end

  fun right (R {init, step, done}) =
    let
      fun init' () = Step.rmap Either.INR (init ())

      fun step' (Either.INR x, s) = Step.rmap Either.INR (step (x, s))
        | step' (Either.INL x, _) = Step.Done (Either.INL x)

      fun done' s = Either.INR (done s)
    in
      R {init = init', step = step', done = done'}
    end

  fun unfirst (R {init, step, done}) =
    let
      datatype 'c result
        = Waiting
        | Strict of 'c
        | Lazy of unit -> 'c

      exception State of 's * 'c result ref

      fun deref r =
        case !r of
            Strict c => c
          | Waiting => raise Fail "Reducer.unfirst: forced too early"
          | Lazy k =>
              let
                val c = k ()
              in
                r := Strict c;
                c
              end

      fun init' () =
        case init () of
            Step.Done (b, _) => Step.Done b
          | Step.More s => Step.More (State (s, ref Waiting))

      fun step' (x, State (s, r)) =
        (case step ((x, fn () => deref r), s) of
             Step.Done (b, c) => (r := Lazy c; Step.Done b)
           | Step.More s' => Step.More (State (s', r)))
        | step' _ = raise Impossible

      fun done' (State (s, r)) =
        let
          val (b, c) = done s
        in
          r := Lazy c;
          b
        end
        | done' _ = raise Impossible
    in
      R {init = init', step = step', done = done'}
    end

  fun swap (a, b) = (b, a)

  fun unsecond r = unfirst (dimap swap swap r)

  fun extract (R {init, done, ...}) =
    case init () of
        Step.Done b => b
      | Step.More s => done s

  fun duplicate (R {init, step, done}) =
    let
      fun init' () = Step.rmap pure (init ())

      fun step' (x, s) = Step.rmap pure (step (x, s))

      fun done' s = R {init = fn () => Step.More s, step = step, done = done}
    in
      R {init = init', step = step', done = done'}
    end

  fun extend f r = rmap f (duplicate r)

  val &&& = both

  local
    fun fst (a, _) = a
    fun snd (_, b) = b
  in
    fun *** (r1, r2) = &&& (lmap fst r1, lmap snd r2)
  end

  fun either (r1, r2) =
    both (premapPartial Either.asLeft r1, premapPartial Either.asRight r2)

  fun compose (r1 : ('s1, 'b, 'c) t', r2 : ('s2, 'a, 'b) t') : ('a, 'c) t =
    let
      val R {init = init1, step = step1, done = done1} = r1
      val R {init = init2, step = step2, done = done2} = r2

      exception NotDone of 's1 * 's2

      fun init () =
        case (init1 (), init2 ()) of
            (Step.Done c, _) => Step.Done c
          | (Step.More s1, Step.More s2) => Step.More (NotDone (s1, s2))
          | (Step.More s1, Step.Done b) =>
              case step1 (b, s1) of
                  Step.Done c => Step.Done c
                | Step.More s1' => Step.Done (done1 s1')

      fun step (x, NotDone (s1, s2)) =
        (case step2 (x, s2) of
            Step.Done b =>
              (case step1 (b, s1) of
                  Step.Done c => Step.Done c
                | Step.More s1' => Step.Done (done1 s1'))
          | Step.More s2' =>
              case step1 (done2 s2', s1) of
                  Step.Done c => Step.Done c
                | Step.More s1' => Step.More (NotDone (s1', s2')))
        | step _ = raise Impossible

      fun done (NotDone (s1, _)) = done1 s1
        | done _ = raise Impossible
    in
      R {init = init, step = step, done = done}
    end

  local
    exception State of int

    fun init () = Step.More (State 0)

    fun step (_, State n) = Step.More (State (n + 1))
      | step _ = raise Impossible

    fun done (State n) = n
      | done _ = raise Impossible
  in
    val length = R {init = init, step = step, done = done}
  end

  local
    exception State
  in
    fun all f = R {
        init = fn () => Step.More State,
        step = fn (x, _) => if f x then Step.More State else Step.Done false,
        done = fn _ => true
      }

    fun exists f = R {
        init = fn () => Step.More State,
        step = fn (x, _) => if f x then Step.Done true else Step.More State,
        done = fn _ => false
      }
  end

  fun fold f z =
    let
      exception State of 'b

      fun init () = Step.More (State z)

      fun step (x, State s) = Step.More (State (f (x, s)))
        | step _ = raise Impossible

      fun done (State s) = s
        | done _ = raise Impossible
    in
      R {init = init, step = step, done = done}
    end

  fun drop n (R {init, step, done}) =
    let
      exception State of int * 's

      fun state m s = State (m, s)

      fun init' () = Step.lmap (state n) (init ())

      fun step' (x, State (0, s)) = Step.lmap (state 0) (step (x, s))
        | step' (x, State (m, s)) = Step.More (State (m - 1, s))
        | step' _ = raise Impossible

      fun done' (State (_, s)) = done s
        | done' _ = raise Impossible
    in
      R {init = init', step = step', done = done'}
    end

  fun take n (R {init, step, done}) =
    let
      exception State of int * 's

      fun state (_, Step.Done b) = Step.Done b
        | state (m, Step.More s) =
        if m <= 0
          then Step.Done (done s)
        else Step.More (State (m, s))

      fun init' () = state (n, init ())

      fun step' (x, State (m, s)) = state (m - 1, step (x, s))
        | step' _ = raise Impossible

      fun done' (State (_, s)) = done s
        | done' _ = raise Impossible
    in
      R {init = init', step = step', done = done'}
    end

end
