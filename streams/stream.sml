
structure Stream : STREAM =
struct

(* datatype void = Void of void *)

(* datatype 'a stream_k = StreamK of *)
(*   { state : 'a stream_k_state, *)
(*     yield : 'a * 'a stream_k -> void, *)
(*     single : 'a -> void, *)
(*     stop : unit -> void *)
(*   } -> void *)

(* and 'a stream_k_state = StreamKState of { *)
(*   } *)

(* val empty = StreamK (fn {stop, ...} => stop ()) *)

(* fun pure a = StreamK (fn {single, ...} => single a) *)

(* fun pureM th = StreamK (fn {single, ...} => single (th ())) *)

(* fun cons (x, s) = StreamK (fn {yield, ...} => yield (x, s)) *)

(* fun consM (th, s) = StreamK (fn {yield, ...} => yield (th (), s)) *)


datatype ('a, 's) step =
    Yield of 'a * 's
  | Skip of 's
  | Stop

datatype 'a stream = Stream of (exn -> ('a, exn) step) * exn

exception True
exception False
exception None
exception Some of exn

fun mapStep f (Yield (a, s)) = Yield (f a, s)
  | mapStep _ (Skip s) = Skip s
  | mapStep _ Stop = Stop

val empty = Stream (fn _ => Stop, Empty)

fun consM (th, Stream (step, state)) = let
      fun step' (Some s) = mapStep Some (step s)
        | step' _ = Yield (th (), Some state)
      in Stream (step', None) end

fun cons (x, Stream (step, state)) = let
      fun step' (Some s) = mapStep Some (step s)
        | step' _ = Yield (x, Some state)
      in Stream (step', None) end

fun pureM th = let
      fun step True = Yield (th (), False)
        | step _ = Stop
      in Stream (step, True) end

fun pure x = let
      fun step True = Yield (x, False)
        | step _ = Stop
      in Stream (step, True) end

end
