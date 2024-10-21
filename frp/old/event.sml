
structure Event: EVENT =
struct

type cancellation = unit -> unit

datatype 'a t = Event of (('a -> unit) -> cancellation)

fun cancel th = th ()

fun subscribe (Event e) k = e k

fun create () =
   let
      val subscribers = ref []
      val counter = ref 0
      fun subscribe k = 
         let
            val n = !counter
         in
            counter := !counter + 1
            ; subscribers := !subscribers @ [(n, k)]
            ; fn () =>
               subscribers :=
                  List.filter
                     (fn (m, _) => n = m)
                     (!subscribers)
         end

      fun push a = List.app (fn (_, k) => k a) (!subscribers)
   in
      {event = Event subscribe, push = push}
   end

fun map f (Event e) = Event (fn k => e (k o f))

val empty = Event (fn _ => fn () => ())

fun pure a = Event (fn k => (k a; fn () => ()))

fun filter p (Event e) = Event (fn k =>
   e (fn a => if p a then k a else ()))

fun compact e = map Option.valOf (filter Option.isSome e)

local
   fun leftVal (Either.INL a) = a
     | leftVal _ = raise Fail "Internal error: leftVal given INR"

   fun rightVal (Either.INR b) = b
     | rightVal _ = raise Fail "Internal error: rightVal given INL"
in
   fun separate e =
      (map leftVal (filter Either.isLeft e),
       map rightVal (filter Either.isRight e))
end

fun mapPartial f e = compact (map f e)

fun apply (Event e1, Event e2) = Event (fn k =>
   let
      val latestA = ref NONE
      val latestB = ref NONE
      val c1 = e1 (fn a =>
         (latestA := SOME a
          ; Option.app (fn b => k (a b)) (!latestB)))
      val c2 = e2 (fn b =>
         (latestB := SOME b
          ; Option.app (fn a => k (a b)) (!latestA)))
   in
      fn () => (c1 (); c2 ())
   end)

fun map2 f e1 e2 =
   apply (map (fn a => fn b => f (a, b)) e1, e2)

fun alt (Event e1, Event e2) = Event (fn k =>
   let
      val c1 = e1 k
      val c2 = e2 k
   in
      fn () => (c1 (); c2 ())
   end)

fun partition p e = (filter p e, filter (not o p) e)

fun partitionEither f e =
   (mapPartial (Either.asLeft o f) e,
    mapPartial (Either.asRight o f) e)

fun fold f b (Event e) = Event (fn k =>
   let
      val result = ref b
   in
      e (fn a =>
         (result := f (a, !result)
          ; k (!result)))
   end)

fun sampleOn (Event e1, Event e2) = Event (fn k =>
   let
      val latest = ref NONE
      val c1 = e1 (fn a => latest := SOME a)
      val c2 = e2 (fn f => Option.app (k o f) (!latest))
   in
      fn () => (c1 (); c2 ())
   end)

fun keepLatest (Event e) = Event (fn k =>
   let
      val cInner = ref NONE
      fun cancelInner () =
         case !cInner of
            NONE => ()
          | SOME th => th ()
      val cancelOuter = e (fn inner =>
         (cancelInner ()
          ; cInner := SOME (subscribe inner k)))
   in
      fn () => (cancelInner (); cancelOuter ())
   end)

fun fix f = Event (fn k =>
   let
      val {event, push} = create ()
      val {input, output} = f event
      val c1 = subscribe input push
      val c2 = subscribe output k
   in
      fn () => (c1 (); c2 ())
   end)

fun count e = fold (fn (_, n) => n + 1) 0 e

fun withLast e =
   let
      fun step (a, NONE) = SOME {now = a, last = NONE}
        | step (a, SOME {now = b, last}) = SOME {now = a, last = SOME b}
   in
      compact (fold step NONE e)
   end

fun mapAccum f acc e = 
   let
      fun step (a, (acc, _)) =
         let
            val (b, acc') = f (a, acc)
         in
            (b, SOME acc')
         end
   in
      mapPartial #2 (fold step (acc, NONE) e)
   end

fun gate (e1, e2) =
   compact
      (sampleOn (alt (pure NONE, map SOME e1),
                 map (fn x => fn SOME true => SOME x | _ => NONE) e2))

end

(* vim: set tw=0 ts=3 sw=3: *)
