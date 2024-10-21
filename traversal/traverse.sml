
signature TRAVERSABLE =
sig
   type 'a t

   type 'a source_state
   type 'a source = 'a source_state -> ('a * 'a source_state) option
   type 'a sink_state
   type ('a, 'b) sink = 'a sink_state * 'a -> ('a sink_state, 'b) Either.either

   val split : 'a t -> 'a source_state * 'a source * 'a sink_state * ('b, 'b t) sink
end

functor Traversable(T : TRAVERSABLE) =
struct
   open T

   fun toList x =
      let
         val (s0, get, _, _) = split x
         fun loop (s, acc) =
            case get s of
               NONE => List.rev acc
             | SOME (x, s') => loop (s', x :: acc)
      in
         loop (s0, [])
      end
end

structure ListTraversable : TRAVERSABLE =
struct
   type 'a t = 'a list

   type 'a source_state = 'a list
   type 'a source = 'a source_state -> ('a * 'a source_state) option
   type 'a sink_state = int * 'a list
   type ('a, 'b) sink = 'a sink_state * 'a -> ('a sink_state, 'b) Either.either

   fun split xs =
      let
         fun get [] = NONE
           | get (y :: ys) = SOME (y, ys)

         fun put ((1, ys), y) = Either.INR (List.rev (y :: ys))
           | put ((n, ys), y) = Either.INL (n - 1, y :: ys)
      in
         (xs, get, (List.length xs, []), put)
      end
end

structure ListTraversable = Traversable(ListTraversable)

(* vim: set tw=0 ts=3 sw=3: *)
