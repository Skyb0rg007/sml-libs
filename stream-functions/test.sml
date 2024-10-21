
structure Test =
struct

infixr 1 >>> <<< >>^ ^>>
infixr 3 &&& ***

structure SF = SF

val op >>> = SF.>>>
val op >>^ = SF.>>^
val op ^>> = SF.^>>
val op <<< = SF.<<<
val op &&& = SF.&&&
val op *** = SF.***

fun average a =
   (SF.identity &&& SF.pre a) >>> SF.arr (fn (x, y) => (x + y) / 2.0)

val counter = SF.scan (fn ((), n) => n + 1) 0

val risingEdge =
   SF.mapAccum
      (fn (cur, prev) => (cur andalso not prev, cur))
      true

fun fallingBall p0 v0 =
   (* velocity = v0 - 9.8t *)
   (* position = p0 + vt *)
   (SF.constant ~9.8 >>> SF.integral >>^ (fn n => n + v0))
   >>> ((average v0 >>> SF.integral >>^ (fn n => n + p0)) &&& SF.identity)

fun bouncingBall p0 v0 =
   SF.switch
      (fallingBall p0 v0
       >>> (SF.identity
            (* Signal when position & velocity are negative *)
            &&& ((fn (p, v) => p <= 0.0 andalso v < 0.0) ^>> risingEdge))
           (* Flip velocity when signal fires *)
       >>^ (fn ((p, v), false) => Either.INR (p, v)
             | ((p, v), true) => Either.INL (bouncingBall p (~v))))

val _: real -> real -> (unit, real * real) SF.t = fallingBall
val _: real -> real -> (unit, real * real) SF.t = bouncingBall
val _: real -> (real, real) SF.t = average
val _: (unit, int) SF.t = counter

val ball : (unit, real * real) SF.t = bouncingBall 100.0 0.0

fun run sf =
   let
      val n = ref 0
   in
      SF.reactimate
         {initial = (),
          input = fn () => (Posix.Process.sleep (Time.fromMilliseconds 2); (0.02, SOME ())),
          output = fn x => (print (x ^ "\n"); n := !n + 1; false)}
         sf
   end

val test1 =
   counter >>> SF.arr Real.fromInt >>> average 0.0 >>> SF.integral >>> SF.arr Real.toString

fun showBall pos = String.concat
   ["\^[[2J\^[[H",
    CharVector.tabulate (Real.trunc (pos / 3.0), fn _ => #"\n"),
    "o\n"]

val test2 =
   (* ball >>> SF.arr (fn (a, b) => Real.toString a ^ "," ^ Real.toString b) *)
   ball >>> SF.arr (fn (a, b) => showBall a)

end

(* vim: set tw=0 ts=3 sw=3: *)
