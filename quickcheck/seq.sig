
signature SEQ =
sig
   type 'a t

   (* We avoid the Monad naming scheme since there are two valid Applicatives
    * `singleton`, `map2`, `concatMap` forms a Monad
    * `repeat`, `zipWith` forms an Applicative
    *)

   val empty: 'a t
   val cons: 'a * 'a t -> 'a t
   val lcons: (unit -> 'a) * 'a t -> 'a t
   val singleton: 'a -> 'a t
   val repeat: 'a -> 'a t
   val concat: 'a t t -> 'a t
   val fromList: 'a list -> 'a t

   val uncons: 'a t -> ('a * 'a t) option
   val head: 'a t -> 'a option
   val isEmpty: 'a t -> bool

   val map: ('a -> 'b) -> 'a t -> 'b t
   val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
   val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
   val filter: ('a -> bool) -> 'a t -> 'a t
   val ap: ('a -> 'b) t * 'a t -> 'b t
   val take: int * 'a t -> 'a t
   val drop: int * 'a t -> 'a t
   val append: 'a t * 'a t -> 'a t
   val concatMap: ('a -> 'b t) -> 'a t -> 'b t
   val zip: 'a t * 'b t -> ('a * 'b) t
   val zipWith: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
   val unfold: ('b -> ('a * 'b) option) -> 'b -> 'a t
end

(* vim: set tw=0 ts=3 sw=3: *)
