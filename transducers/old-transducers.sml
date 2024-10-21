
signature REDUCER =
   sig
      type ('s, 'a, 'b) t

      val toList : ('a list, 'a, 'a list) t
      val applyTextIOLines : ('s, string, 'b) t -> TextIO.instream -> 'b
      val applyList : ('s, 'a, 'b) t -> 'a list -> 'b
      val reduce : ('a * 'b -> 'b) -> 'b -> ('b, 'a, 'b) t
      val sum : (int, int, int) t
      val product : (int, int, int) t

      val map : ('b -> 'c) -> ('s, 'a, 'b) t -> ('s, 'a, 'c) t
      val map2 : ('b * 'c -> 'd) -> ('s, 'a, 'b) t * ('t, 'a, 'c) t -> ('s * 't, 'a, 'd) t
      val pure : 'b -> (unit, 'a, 'b) t
      val extract : ('s, 'a, 'b) t -> 'b
      val duplicate : ('s, 'a, 'b) t -> ('s, 'a, ('s, 'a, 'b) t) t
      val contramap : ('a -> 'b) -> ('s, 'b, 'c) t -> ('s, 'a, 'c) t
   end

signature TRANSDUCER =
   sig
      structure Reducer : REDUCER

      type ('s, 'a, 'b) step

      val toList : ('a list, 'a, 'a list) step
      val dedup : ('a * 'a -> bool) -> ('s, 'a, 'b) step -> ('s, 'a, 'b) step
      val contramap : ('a -> 'b) -> ('s, 'b, 'c) step -> ('s, 'a, 'c) step
      val filter : ('a -> bool) -> ('s, 'a, 'b) step -> ('s, 'a, 'b) step
      val take : int -> ('s, 'a, 'b) step -> ('s, 'a, 'b) step

      val reduceList : 'a list -> ('s, 'a, 'b) step -> 'b
      val transduce : (('a list, 'a, 'a list) step -> ('s, 'b, 'c) step) -> 'b list -> 'c 
   end

structure Transducers : TRANSDUCER =
struct

structure Reducer =
   struct
      datatype ('s, 'a, 'b) t = Reducer of {
            init: unit -> 's,
            complete: 's -> 'b,
            step: 'a * 's -> 's * bool
         }

      val toList = Reducer {
         init = fn () => [],
         complete = List.rev,
         step = fn (x, xs) => (x :: xs, false)
      }

      fun applyTextIOLines (Reducer {init, complete, step}) stream =
         let
            fun go s =
               case TextIO.inputLine stream of
                  NONE => complete s
                | SOME line =>
                     case step (line, s) of
                        (s', true) => complete s'
                      | (s', false) => go s'
         in
            go (init ())
         end

      fun applyList (Reducer {init, complete, step}) lst =
         let
            fun go (s, []) = complete s
              | go (s, x :: xs) =
               case step (x, s) of
                  (s', true) => complete s'
                | (s', false) => go (s', xs)
         in
            go (init (), lst)
         end

      fun reduce f z = Reducer {
            init = fn () => z,
            complete = fn n => n,
            step = fn (x, acc) => (f (x, acc), false)
         }

      val sum = reduce op + 0
      val product = reduce op * 1

      fun contramap f (Reducer {init, complete, step}) =
         Reducer {
            init = init,
            complete = complete,
            step = fn (x, acc) => step (f x, acc)
         }

      fun map f (Reducer {init, complete, step}) =
         Reducer {
            init = init,
            complete = fn s => f (complete s),
            step = step
         }

      (* fun left (Reducer {init, complete, step}) = *)
      (*    Reducer { *)
      (*       init = fn () => Either.INR (init ()), *)
      (*       complete = Either.mapRight complete, *)
      (*       step = fn (Either.INL a, _) => (Either.INL a, true) *)
      (*               | (Either.INR a, Either.INR s) => *)
      (*                    let *)
      (*                       val (s', done) = step (a, s) *)
      (*                    in *)
      (*                       (Either.INR s', done) *)
      (*                    end *)
      (*               | (_, Either.INL _) => raise Fail "Violated reducer laws" *)
      (*    } *)

      fun pure x = Reducer {
            init = fn () => (),
            complete = fn () => x,
            step = fn (_, ()) => ((), true)
         }

      fun extract (Reducer {init, complete, step}) = complete (init ())

      fun duplicate (Reducer {init, complete, step}) =
         Reducer {
            init = init,
            complete = fn s => Reducer {init = fn () => s, complete = complete, step = step},
            step = step
         }

      fun map2 f (Reducer r1, Reducer r2) =
         Reducer {
            init = fn () => (#init r1 (), #init r2 ()),
            complete = fn (s1, s2) => f (#complete r1 s1, #complete r2 s2),
            step = fn (a, (s1, s2)) =>
               let
                  val (s1', done1) = #step r1 (a, s1)
                  val (s2', done2) = #step r2 (a, s2)
               in
                  ((s1', s2'), done1 orelse done2)
               end
         }
   end

type ('s, 'a, 'b) reducer = ('s, 'a, 'b) Reducer.t

datatype ('s, 'a, 'b) step = Step of {
      init: unit -> 's,
      complete: 's -> 'b,
      step: 's * 'a -> 's * bool (* state', done *)
   }

val toList = Step {
      init = fn () => [],
      complete = List.rev,
      step = fn (xs, x) => (x :: xs, false)
   }

fun dedup same (Step {init, complete, step}) =
   let
      val prev = ref NONE
      fun same' (_, NONE) = false
        | same' (x, SOME y) = same (x, y)
   in
      Step {
         init = init,
         complete = complete,
         step = fn (s, a) =>
            if same' (a, !prev)
               then (s, false)
            else (prev := SOME a; step (s, a))
      }
   end

fun contramap f (Step {init, complete, step}) =
   Step {
      init = init,
      complete = complete,
      step = fn (s, a) => step (s, f a)
   }

fun filter p (Step {init, complete, step}) =
   Step {
      init = init,
      complete = complete,
      step = fn (s, a) =>
         if p a
            then step (s, a)
         else (s, false)
   }

fun take n (Step {init, complete, step}) =
   let
      val r = ref n
   in
      Step {
         init = init,
         complete = complete,
         step = fn (s, a) =>
            let
               val m = !r
               val () = r := m - 1
               val res = if m > 0 then step (s, a) else (s, true)
            in
               if m <= 0
                  then (#1 res, true)
               else res
            end
      }
   end

fun reduceList lst (Step {init, complete, step}) =
   let
      fun loop (s, []) = complete s
        | loop (s, x :: xs) =
         case step (s, x) of
            (s', true) => complete s'
          | (s', false) => loop (s', xs)
   in
      loop (init (), lst)
   end

fun transduce t coll =
   reduceList coll (t toList)

end

(* vim: set tw=0 ts=3 sw=3: *)
