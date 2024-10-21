
signature PROXY =
sig

type 'a m
type ('a2, 'a1, 'b1, 'b2, 'r) t

(* Effects can't request or respond *)
type 'r effect = (Void.t, unit, Void.t, unit, 'r) t
(* Producers can only respond *)
type ('b, 'r) producer = (Void.t, unit, unit, 'b, 'r) t
(* Pipes can request and respond *)
type ('a, 'b, 'r) pipe = (unit, 'a, unit, 'b, 'r) t
(* Consumers can only request *)
type ('a, 'r) consumer = (unit, 'a, unit, Void.t, 'r) t
(* Clients send requests of type 'a2 and recieve responses of type 'a1 *)
type ('a2, 'a1, 'r) client = ('a2, 'a1, unit, Void.t, 'r) t
(* Servers recieve requests of type 'b2 and send responses of type 'b1 *)
type ('b2, 'b1, 'r) server = (Void.t, unit, 'b2, 'b1, 'r) t

val lift: 'a m -> ('a2, 'a1, 'b1, 'b2, 'a) t
val reflect: ('a2, 'a1, 'b2, 'b1, 'r) t -> ('b1, 'b2, 'a1, 'a2, 'r) t
val run: (Void.t, 'b, 'c, Void.t, 'a) t -> 'a m

structure Kleisli:
   sig
      val pure: 'r -> ('a2, 'a1, 'b2, 'b1, 'r) t

      val map: ('r1 -> 'r2) -> ('a2, 'a1, 'b2, 'b1, 'r1) t -> ('a2, 'a1, 'b2, 'b1, 'r2) t

      val >>= : ('a2, 'a1, 'b2, 'b1, 'r1) t
              * ('r1 -> ('a2, 'a1, 'b2, 'b1, 'r2) t)
              -> ('a2, 'a1, 'b2, 'b1, 'r2) t

      val >=> : ('r1 -> ('a2, 'a1, 'b2, 'b1, 'r2) t)
              * ('r2 -> ('a2, 'a1, 'b2, 'b1, 'r3) t)
             -> ('r1 -> ('a2, 'a1, 'b2, 'b1, 'r3) t)

      val =<< : ('r1 -> ('a2, 'a1, 'b2, 'b1, 'r2) t)
              * ('a2, 'a1, 'b2, 'b1, 'r1) t
              -> ('a2, 'a1, 'b2, 'b1, 'r2) t
      val <=< : ('r2 -> ('a2, 'a1, 'b2, 'b1, 'r3) t)
              * ('r1 -> ('a2, 'a1, 'b2, 'b1, 'r2) t)
             -> ('r1 -> ('a2, 'a1, 'b2, 'b1, 'r3) t)
   end

structure Respond:
   sig
      val pure: 'a1 -> ('x2, 'x1, 'a2, 'a1, 'a2) t

      val >>= : ('x2, 'x1, 'b2, 'b1, 'a2) t
              * ('b1 -> ('x2, 'x1, 'c2, 'c1, 'b2) t)
             -> ('x2, 'x1, 'c2, 'c1, 'a2) t

      val >=> : ('a1 -> ('x2, 'x1, 'b2, 'b1, 'a2) t)
              * ('b1 -> ('x2, 'x1, 'c2, 'c1, 'b2) t)
             -> ('a1 -> ('x2, 'x1, 'c2, 'c1, 'a2) t)
   end

structure Request:
   sig
      val pure: 'a2 -> ('a2, 'a1, 'y2, 'y1, 'a1) t

      val =<< : ('b2 -> ('a2, 'a1, 'y2, 'y1, 'b1) t)
              * ('b2, 'b1, 'y2, 'y1, 'c) t
             -> ('a2, 'a1, 'y2, 'y1, 'c) t

      val <=< : ('b2 -> ('a2, 'a1, 'y2, 'y1, 'b1) t)
              * ('c2 -> ('b2, 'b1, 'y2, 'y1, 'c1) t)
             -> ('c2 -> ('a2, 'a1, 'y2, 'y1, 'c1) t)
   end

structure Push:
   sig
      val pure: 'a1 -> ('a2, 'a1, 'a2, 'a1, 'r) t

      val >>= : ('a2, 'a1, 'b2, 'b1, 'r) t
              * ('b1 -> ('b2, 'b1, 'c2, 'c1, 'r) t)
             -> ('a2, 'a1, 'c2, 'c1, 'r) t

      val >=> : ('a  -> ('a2, 'a1, 'b2, 'b1, 'r) t)
              * ('b1 -> ('b2, 'b1, 'c2, 'c1, 'r) t)
             -> ('a  -> ('a2, 'a1, 'c2, 'c1, 'r) t)
   end

structure Pull:
   sig
      val pure: 'a2 -> ('a2, 'a1, 'a2, 'a1, 'r) t

      val =<< : ('b2 -> ('a2, 'a1, 'b2, 'b1, 'r) t)
              * ('b2, 'b1, 'c2, 'c1, 'r) t
             -> ('a2, 'a1, 'c2, 'c1, 'r) t

      val <=< : ('b2 -> ('a2, 'a1, 'b2, 'b1, 'r) t)
              * ('c  -> ('b2, 'b1, 'c2, 'c1, 'r) t)
             -> ('c  -> ('a2, 'a1, 'c2, 'c1, 'r) t)
   end

structure Prelude:
   sig
      val take: int -> ('a, 'a, unit) pipe
      val drop: int -> ('a, 'a, 'r) pipe
      val takeWhile: ('a -> bool) -> ('a, 'a, unit) pipe
      val takeWhile': ('a -> bool) -> ('a, 'a, 'a) pipe
      val dropWhile: ('a -> bool) -> ('a, 'a, 'r) pipe
      val replicate: int -> 'a m -> ('x2, 'x1, unit, 'a, unit) t
      val fromList: 'a list -> ('x2, 'x1, unit, 'a, unit) t
      val findIndices: ('a -> bool) -> ('a, int, 'r) pipe
      val next: ('a, 'r) producer -> (('r, 'a * ('a, 'r) producer) Either.either) m
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
