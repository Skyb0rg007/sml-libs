
signature FUNCTOR =
   sig
      type 'a t

      val map: ('a -> 'b) -> 'a t -> 'b t
   end

signature APPLY =
   sig
      include FUNCTOR

      val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
      val both: 'a t * 'b t -> ('a * 'b) t
   end

signature APPLICATIVE =
   sig
      include APPLY

      val pure: 'a -> 'a t
   end

signature COAPPLY =
   sig
      include FUNCTOR

      val either: ('a, 'b) Either.either t -> ('a t, 'b t) Either.either
   end

signature COAPPLICATIVE =
   sig
      include COAPPLY

      val copure: 'a t -> 'a
   end

signature DISTRIBUTIVE =
   sig
      include FUNCTOR

      type x

      val sub: 'a t -> x -> 'a
      val tabulate: (x -> 'a) -> 'a t
   end

(* vim: set tw=0 ts=3 sw=3: *)
