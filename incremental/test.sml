
structure Test =
struct

structure N = Node
open FRP

val (e: int Event.t, fire) = Event.new ()

val e' = Event.map (fn n => n + 1) e

val () = Event.subscribe e (fn x => print ("e = " ^ Int.toString x ^ "\n"))
val () = Event.subscribe e' (fn x => print ("e' = " ^ Int.toString x ^ "\n"))

val () = fire 1
val () = fire 2
val () = fire 3

val () = cleanup ()

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
