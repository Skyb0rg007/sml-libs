
signature REDUCER =
sig
   datatype 'a reduced =
      Reduced of 'a
    | Continue of 'a

   datatype ('s, 'a, 'b) t = T of {
      init: 's,
      step: 'a * 's -> 's reduced,
      done: 's -> 'b
   }

   (* Implementation detail *)
   type ('s, 't) ap_state
   type ('b, 's, 't) o_state

   val reduceList: ('s, 'a, 'b) t -> 'a list -> 'b

   (* Functor *)
   val map: ('b -> 'c) -> ('s, 'a, 'b) t -> ('s, 'a, 'c) t

   (* Profunctor *)
   val lmap: ('c -> 'a) -> ('s, 'a, 'b) t -> ('s, 'c, 'b) t

   (* Choice profunctor *)
   val left: ('s, 'a, 'b) t -> (('s, 'c) Either.either, ('a, 'c) Either.either, ('b, 'c) Either.either) t
   val right: ('s, 'a, 'b) t -> (('c, 's) Either.either, ('c, 'a) Either.either, ('c, 'b) Either.either) t

   (* Applicative *)
   val pure: 'b -> (unit, 'a, 'b) t
   val ap: ('s, 'a, 'b -> 'c) t * ('t, 'a, 'b) t -> (('s, 't) ap_state, 'a, 'c) t
   val map2: ('b * 'c -> 'd) -> ('s, 'a, 'b) t * ('t, 'a, 'c) t -> (('s, 't) ap_state, 'a, 'd) t

   (* Comonad *)
   val extract: ('s, 'a, 'b) t -> 'b
   val duplicate: ('s, 'a, 'b) t -> ('s, 'a, ('s, 'a, 'b) t) t
   val extend: ('s, 'a, 'b) t -> (('s, 'a, 'b) t -> 'c) -> ('s, 'a, 'c) t

   (* Semigroupoid *)
   val o: ('s, 'b, 'c) t * ('t, 'a, 'b) t -> (('b, 's, 't) o_state, 'a, 'c) t
end
(* vim: set ft=sml tw=0 sw=3 ts=3: *)
