
structure Pipes =
struct

structure IO =
   struct
      type 'a t = unit -> 'a
      fun pure x () = x
      fun map f x () = f (x ())
      fun >>= (x, f) () = f (x ()) ()
   end

structure Proxy = ProxyFn(IO)

(* Producers *)
val yield = Proxy.Respond.pure
val for = Proxy.Respond.>>=

(* Consumers *)
val await = Proxy.Request.pure

fun fromTextIO stream =
   let
      open Proxy.Kleisli
      infix 1 >>=
   in
      Proxy.lift (fn () => TextIO.inputLine stream) >>=
      (fn NONE => pure ()
        | SOME str => yield str >>= (fn () => fromTextIO stream))
   end

fun triple x =
   let
      val yield = Proxy.Respond.pure
      val >>= = Proxy.Kleisli.>>=
      infix 1 >>=
   in
      yield x >>= (fn _ => yield x >>= (fn _ => yield x))
   end

val loop: (string, unit) Proxy.producer = Proxy.Respond.>>= (fromTextIO TextIO.stdIn, triple)

fun main () =
   Proxy.run (Proxy.Respond.>>= (loop, fn s => Proxy.lift (fn () => TextIO.print s))) ()

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
