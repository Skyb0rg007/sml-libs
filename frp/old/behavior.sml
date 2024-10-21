
structure Behavior: BEHAVIOR =
struct

datatype ('a, 'b) t = Behavior of ('a -> 'b) Event.t -> 'b Event.t

fun map f (Behavior b) = Behavior (fn e =>
   b (Event.map (fn g => g o f) e))

fun apply (Behavior f, Behavior a) = Behavior (fn e =>
   a (f (Event.map (Fn.curry op o) e)))

fun pure a = Behavior (fn e => Event.map (fn f => f a) e)

fun step a e = Behavior (fn e' => Event.sampleOn (Event.alt (Event.pure a, e), e'))

fun sample (Behavior b) e = b e

fun sampleBy f b e = sample (map (Fn.curry f) b) (Event.map (fn x => fn f => f x) e)

fun sample_ b e = sampleBy #1 b e

fun switcher b0 e = Behavior (fn s =>
   Event.keepLatest (Event.alt (Event.pure (sample b0 s),
                                Event.map (fn b => sample b s) e)))

fun fix a f = Behavior (fn s =>
   Event.fix (fn e =>
      let
         val b = f (step a e)
      in
         {input = sample_ b s, output = Event.sampleOn (e, s)}
      end))

end

(* vim: set tw=0 ts=3 sw=3: *)
