
signature PROFUNCTOR =
   sig
      type ('a, 'b) t

      val dimap: ('c -> 'a) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
   end

signature CHOICE =
   sig
      include PROFUNCTOR

      val left: ('a, 'b) t -> (('a, 'c) Either.either, ('b, 'c) Either.either) t
   end

signature COCHOICE =
   sig
      include PROFUNCTOR

      val unleft: (('a, 'c) Either.either, ('b, 'c) Either.either) t -> ('a, 'b) t
   end

signature STRONG =
   sig
      include PROFUNCTOR

      val first: ('a, 'b) t -> ('a * 'c, 'b * 'c) t
   end

signature COSTRONG =
   sig
      include PROFUNCTOR

      val unfirst: ('a * 'c, 'b * 'c) t -> ('a, 'b) t
   end

signature CLOSED =
   sig
      include PROFUNCTOR

      val closed: ('a, 'b) t -> ('c -> 'a, 'c -> 'b) t
   end

signature REPRESENTABLE =
   sig
      include PROFUNCTOR

      structure F: FUNCTOR

      val sieve: ('a, 'b) t -> 'a -> 'b F.t
      val tabulate: ('a -> 'b F.t) -> ('a, 'b) t
   end

signature COREPRESENTABLE =
   sig
      include PROFUNCTOR

      structure F: FUNCTOR

      val sieve: ('a, 'b) t -> 'a F.t -> 'b
      val cotabulate: ('a F.t -> 'b) -> ('a, 'b) t
   end

signature TRAVERSING =
   sig
      include PROFUNCTOR

      structure F: APPLICATIVE

      val sieve: ('a, 'b) t -> 'a -> 'b F.t
      val tabulate: ('a -> 'b F.t) -> ('a, 'b) t
   end

signature COTRAVERSING =
   sig
      include PROFUNCTOR

      structure F: APPLICATIVE

      val cosieve: ('a, 'b) t -> 'a F.t -> 'b
      val cotabulate: ('a F.t -> 'b) -> ('a, 'b) t
   end

signature MAPPING =
   sig
      include TRAVERSING

      val roam: (('a -> 'b) * 's -> 't) -> ('a, 'b) t -> ('s, 't) t
      val closed: ('a, 'b) t -> ('x -> 'a, 'x -> 'b) t
   end


(*****************************************************************************)

functor Star(F: FUNCTOR) =
   struct
      type ('a, 'b) t = 'a -> 'b F.t

      structure F = F

      fun first p (a, c) = F.map (fn b => (b, c)) (p a)

      fun sieve p = p
      fun tabulate p = p
   end



(* vim: set tw=0 ts=3 sw=3: *)
