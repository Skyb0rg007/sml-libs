
signature GEN =
sig
   type 'a t

   val generate: Random.rand * int * 'a t -> 'a list
   val generate1: Random.rand * 'a t -> 'a
   val generateTree: Random.rand * 'a t -> 'a Tree.t

   val pure: 'a -> 'a t
   val map: ('a -> 'b) -> 'a t -> 'b t
   val ap: ('a -> 'b) t * 'a t -> 'b t
   val map2: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
   val map3: ('a * 'b * 'c -> 'd) -> 'a t * 'b t * 'c t -> 'd t
   val bind: 'a t -> ('a -> 'b t) -> 'b t
   val sequence: 'a t list -> 'a list t
   val delay: (unit -> 'a t) -> 'a t
   val fix: (('a -> 'b t) -> 'a -> 'b t) -> 'a -> 'b t
   val replicate: int * 'a t -> 'a list t
   val pair: 'a t * 'b t -> ('a * 'b) t
   val join: 'a t t -> 'a t

   val nat: int t
   val smallNat: int t
   val bigNat: int t

   val int: int t
   (* val negInt: int t *)
   (* val smallInt: int t *)
   val intRange: {origin: int, min: int, max: int} -> int t
   (* val bigInt: int t *)
   val unit: unit t
   val bool: bool t (* Shrinks to false *)
   (* val real: real t *)
   (* val option: {ratio: real} -> 'a t -> 'a option t *)
   val oneof: 'a t list -> 'a t
   val element: 'a list -> 'a t
   val frequency: (int * 'a t) list -> 'a t
   (* val charRange: {origin: char} -> char * char -> char t *)

   val make: {gen: Random.rand -> 'a, shrink: 'a -> 'a Seq.t} -> 'a t
end
(* vim: set tw=0 ts=3 sw=3: *)
