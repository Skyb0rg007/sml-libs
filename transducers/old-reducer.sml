
structure Reducer : REDUCER (* : COREPRESENTABLE *) =
struct
  type uni = Universal.universal
  datatype ('a, 'b) t = R of {done : uni -> 'b,
                              init : unit -> bool * uni,
                              step : 'a * uni -> bool * uni}

  fun make {done, init, step} =
    let
      val tag = Universal.tag ()
      val inj = Universal.tagInject tag
      val prj = Universal.tagProject tag
      fun injNext (b, s) = (b, inj s)
      fun done' s = done (prj s)
      fun init' () = injNext (init ())
      fun step' (x, s) = injNext (step (x, prj s))
    in
      R {done = done', init = init', step = step'}
    end

  fun lmap f (R {done, init, step}) =
    R {done = done, init = init, step = fn (x, s) => step (f x, s)}

  fun rmap f (R {done, init, step}) =
    R {done = f o done, init = init, step = step}

  fun dimap f g (R {done, init, step}) =
    R {done = g o done, init = init, step = fn (x, s) => step (f x, s)}

  structure F =
    struct
      type 'a t = 'a list

      val map = List.map
    end

  fun cosieve (R {done, init, step}) lst =
    let
      fun app ([], s) = s
        | app (x :: xs, s) =
        let
          val (b, s') = step (x, s)
        in
          if b then s' else app (xs, s')
        end

      val (b, s) = init ()
    in
      if b
        then done s
      else done (app (lst, s))
    end

  fun cotabulate f =
    let
      fun done lst = f (List.rev lst)
      fun init () = (false, [])
      fun step (x, lst) = (false, x :: lst)
    in
      make {done = done, init = init, step = step}
    end

  local
    val tag = Universal.tag ()
    val unit = Universal.tagInject tag ()
  in
    fun pure b =
      let
        fun done _ = b
        fun init () = (true, unit)
        fun step _ = raise Fail "Reducer.pure: Called step after termination"
      in
        R {done = done, init = init, step = step}
      end
  end

  fun map2 f (r1, r2) =
    let
      val R {done = done1, init = init1, step = step1} = r1
      val R {done = done2, init = init2, step = step2} = r2

      fun done (_, _, s1, s2) = f (done1 s1, done2 s2)

      fun init () =
        let
          val (b1, s1) = init1 ()
          val (b2, s2) = init2 ()
        in
          (b1 andalso b2, (b1, b2, s1, s2))
        end

      fun step (x, (b1, b2, s1, s2)) =
        let
          val (b1', s1') = if b1 then (true, s1) else step1 (x, s1)
          val (b2', s2') = if b2 then (true, s2) else step2 (x, s2)
        in
          (b1' andalso b2', (b1', b2', s1', s2'))
        end
    in
      make {done = done, init = init, step = step}
    end

  fun left (R {done, init, step}) =
    let
      fun mapSnd f (x, y) = (x, f y)
      fun init' () = mapSnd Either.INL (init ())
      val done' = Either.mapLeft done
      fun step' (Either.INR x, _) = (true, Either.INR x)
        | step' (_, Either.INR x) = (true, Either.INR x)
        | step' (Either.INL x, Either.INL s) = mapSnd Either.INL (step (x, s))
    in
      make {done = done', init = init', step = step'}
    end

  fun right (R {done, init, step}) =
    let
      fun mapSnd f (x, y) = (x, f y)
      fun init' () = mapSnd Either.INR (init ())
      val done' = Either.mapRight done
      fun step' (Either.INL x, _) = (true, Either.INL x)
        | step' (_, Either.INL x) = (true, Either.INL x)
        | step' (Either.INR x, Either.INR s) = mapSnd Either.INR (step (x, s))
    in
      make {done = done', init = init', step = step'}
    end

  fun extract (R {done, init, ...}) =
    done (#2 (init ()))

  (* XXX: This is wrong! State may be reduced! *)
  fun duplicate (R {done, init, step}) =
    let
      fun done' s = R {done = done, init = fn () => (false, s), step = step}
    in
      R {done = done', init = init, step = step}
    end
end
