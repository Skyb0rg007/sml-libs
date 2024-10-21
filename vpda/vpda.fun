
functor RunVPDA(M: VPDA):
   sig
      datatype input =
         Call of M.Input.call
       | Return of M.Input.return
       | Internal of M.Input.internal

      val run: input list -> bool
      val runTraced: (M.state -> string) -> (M.stack -> string) -> (input -> string) -> input list -> bool
   end =
   struct
      datatype input =
         Call of M.Input.call
       | Return of M.Input.return
       | Internal of M.Input.internal

      fun runTraced prState prStack prInput input =
         let
            fun go' (state, stack, Call c :: input) =
               let
                  val (state', sym) = M.transition_call (state, c)
               in
                  go (state', sym :: stack, input)
               end
              | go' (state, [], Return r :: input) =
               go (M.transition_return (state, r, NONE), [], input)
              | go' (state, sym :: stack, Return r :: input) =
               go (M.transition_return (state, r, SOME sym), stack, input)
              | go' (state, stack, Internal i :: input) =
               go (M.transition_internal (state, i), stack, input)
              | go' (state, _, []) = M.accepting state

            and go (state, stack, input) =
               (* (print ("state = " ^ prState state ^ "\n"); *)
               (*  print ("stack = [" ^ String.concatWith "," (List.map prStack stack) ^ "]\n"); *)
               (*  print ("input = [" ^ String.concatWith "," (List.map prInput input) ^ "]\n\n"); *)
               (
                go' (state, stack, input))
         in
            go (M.start_state, [], input)
         end

      fun run input = runTraced (fn _ => ".") (fn _ => ".") (fn _ => ".") input
   end

functor NDVPDA_Union(
      structure M1: NDVPDA
      structure M2: NDVPDA
         sharing M1.Input = M2.Input
   ): NDVPDA =
   struct
      structure Input = M1.Input

      type state = (M1.state, M2.state) Either.either
      type stack = (M1.stack, M2.stack) Either.either

      val all_states =
         List.map Either.INL M1.all_states @ List.map Either.INR M2.all_states

      fun same_state (Either.INL x, Either.INL y) = M1.same_state (x, y)
        | same_state (Either.INR x, Either.INR y) = M2.same_state (x, y)
        | same_state _ = false

      val start_states =
         List.map Either.INL M1.start_states @ List.map Either.INR M2.start_states

      fun accepting (Either.INL x) = M1.accepting x
        | accepting (Either.INR x) = M2.accepting x

      fun transition_call (Either.INL p, c) =
         List.map
            (fn (q, g) => (Either.INL q, Either.INL g))
            (M1.transition_call (p, c))
        | transition_call (Either.INR p, c) =
         List.map
            (fn (q, g) => (Either.INR q, Either.INR g))
            (M2.transition_call (p, c))

      fun transition_return (Either.INL p, r, SOME (Either.INL g)) =
         List.map Either.INL (M1.transition_return (p, r, SOME g))
        | transition_return (Either.INL p, r, NONE) =
         List.map Either.INL (M1.transition_return (p, r, NONE))
        | transition_return (Either.INR p, r, SOME (Either.INR g)) =
         List.map Either.INR (M2.transition_return (p, r, SOME g))
        | transition_return (Either.INR p, r, NONE) =
         List.map Either.INR (M2.transition_return (p, r, NONE))
        | transition_return _ = []

      fun transition_internal (Either.INL p, i) =
         List.map Either.INL (M1.transition_internal (p, i))
        | transition_internal (Either.INR p, i) =
         List.map Either.INR (M2.transition_internal (p, i))
   end

functor VPDA_Intersection(
      structure M1: VPDA
      structure M2: VPDA
         sharing M1.Input = M2.Input
   ): VPDA =
   struct
      structure Input = M1.Input

      type state = M1.state * M2.state
      type stack = M1.stack * M2.stack

      val start_state =
         (M1.start_state, M2.start_state)

      fun accepting (q1, q2) =
         M1.accepting q1 andalso M2.accepting q2

      fun transition_call ((p1, p2), c) =
         let
            val (q1, g1) = M1.transition_call (p1, c)
            val (q2, g2) = M2.transition_call (p2, c)
         in
            ((q1, q2), (g1, g2))
         end

      fun transition_return ((p1, p2), r, SOME (g1, g2)) =
         (M1.transition_return (p1, r, SOME g1),
          M2.transition_return (p2, r, SOME g2))
        | transition_return ((p1, p2), r, NONE) =
         (M1.transition_return (p1, r, NONE),
          M2.transition_return (p2, r, NONE))

      fun transition_internal ((p1, p2), i) =
         (M1.transition_internal (p1, i),
          M2.transition_internal (p2, i))
   end

functor Determinize(M: NDVPDA): VPDA =
   struct
      structure Input = M.Input

      type state = (M.state * M.state) list * M.state list

      type stack = state * Input.call

      val idq = List.map (fn q => (q, q)) M.all_states

      val all_states = []

      val start_state = (idq, M.start_states)

      fun accepting (_, r) = List.exists M.accepting r

      fun transition_call ((s, r): state, a: Input.call): state * stack =
         let
            val r' = List.concat (List.map (fn q => List.map #1 (M.transition_call (q, a))) r)
         in
            ((idq, r'), ((s, r), a))
         end

      fun transition_return ((s, r), a, NONE) =
         let
            val s' = List.concat (List.map (fn (q, q'') => List.map (fn q' => (q, q')) (M.transition_return (q'', a, NONE))) s)
            val r' = List.concat (List.map (fn q => M.transition_return (q, a, NONE)) r)
         in
            (s', r')
         end
        | transition_return ((s, r), a, SOME ((s', r'), a')) =
         let
            fun concatMap f x = List.concat (List.map f x)

            val update: (M.state * M.state) list =
               (concatMap
                  (fn q =>
                     concatMap
                        (fn (q1, g) =>
                           case List.find (fn (q1', _) => M.same_state (q1, q1')) s of
                              NONE => []
                            | SOME (_, q2) => 
                                 List.map
                                    (fn q' => (q, q'))
                                    (M.transition_return (q2, a, SOME g)))
                        (M.transition_call (q, a')))
                  M.all_states)
            val s'' =
               List.mapPartial
                  (fn (q, q3) =>
                     case List.find (fn (q3', _) => M.same_state (q3, q3')) update of
                        NONE => NONE
                      | SOME (_, q') => SOME (q, q'))
                  s'
            val r'' = List.map #2 update
         in
            (s'', r'')
         end

      fun transition_internal ((s, r), a) =
         let
            val s' = List.concat (List.map (fn (q, q'') => let val q's = M.transition_internal (q'', a) in List.map (fn q' => (q, q')) q's end) s)
            val r' = List.concat (List.map (fn q => M.transition_internal (q, a)) r)
         in
            (s', r')
         end
   end

functor VPDA_Union(
      structure M1: VPDA
      structure M2: VPDA
         sharing M1.Input = M2.Input
   ) =
   struct
      structure Input = M1.Input
      structure E = Either

      type state' = (M1.state, M2.state) E.either
      type state = (state' -> state' option) * state' list

      type stack = state * Input.call

      val start_state = (SOME, [E.INL M1.start_state, E.INR M2.start_state])

      fun accepting (_, r) =
         List.exists
            (fn E.INL q => M1.accepting q
              | E.INR q => M2.accepting q)
            r

      fun transition_internal ((s, r), a) =
         let
            fun s' q =
               case s q of
                  NONE => NONE
                | SOME (E.INL q'') => SOME (E.INL (M1.transition_internal (q'', a)))
                | SOME (E.INR q'') => SOME (E.INR (M2.transition_internal (q'', a)))

            val r' =
               List.map
                  (fn E.INL q => E.INL (M1.transition_internal (q, a))
                    | E.INR q => E.INR (M2.transition_internal (q, a)))
                  r
         in
            (s', r')
         end

      fun transition_call ((s, r), a) =
         let
            val r' =
               List.map
                  (fn E.INL q => E.INL (#1 (M1.transition_call (q, a)))
                    | E.INR q => E.INR (#1 (M2.transition_call (q, a))))
                  r
         in
            ((SOME, r'), ((s, r), a))
         end

      fun transition_return ((s, r), a, NONE) =
         let
            fun s' q =
               case s q of
                  SOME (E.INL q'') => SOME (E.INL (M1.transition_return (q'', a, NONE)))
                | SOME (E.INR q'') => SOME (E.INR (M2.transition_return (q'', a, NONE)))
                | NONE => NONE

            val r' =
               List.map
                  (fn E.INL q => E.INL (M1.transition_return (q, a, NONE))
                    | E.INR q => E.INR (M2.transition_return (q, a, NONE)))
                  r
         in
            (s', r')
         end
        | transition_return ((s, r), a, SOME ((s', r'), a')) =
         let
            fun update (E.INL q) =
               let
                  val (q1, g) = M1.transition_call (q, a')
               in
                  case s (E.INL q1) of
                     SOME (E.INL q2) => SOME (E.INL (M1.transition_return (q2, a, SOME g)))
                   | _ => NONE
               end
              | update (E.INR q) =
               let
                  val (q1, g) = M2.transition_call (q, a')
               in
                  case s (E.INR q1) of
                     SOME (E.INR q2) => SOME (E.INR (M2.transition_return (q2, a, SOME g)))
                   | _ => NONE
               end

            val s'' = Option.composePartial (update, s')
            val r'' = List.mapPartial update r'
         in
            (s'', r'')
         end
   end

(* vim: set tw=0 ts=3 sw=3: *)
