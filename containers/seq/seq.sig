
signature SEQ =
sig

datatype 'a node =
   Nil
 | Cons of 'a * (unit -> 'a node)

type 'a t = unit -> 'a node

(* Constructing sequences *)
val empty: 'a t
val singleton: 'a -> 'a t
val cons: 'a * 'a t -> 'a t
val tabulate: int * (int -> 'a) -> 'a t
val unfold: ('b -> ('a * 'b) option) -> 'b -> 'a t
val repeat: 'a -> 'a t
val forever: (unit -> 'a) -> 'a t
val cycle: 'a t -> 'a t
val iterate: ('a -> 'a) -> 'a -> 'a t
val append: 'a t * 'a t -> 'a t

(* Consuming sequences *)
val isEmpty: 'a t -> bool
val uncons: 'a t -> ('a * 'a t) option
val length: 'a t -> int
val app: ('a -> unit) -> 'a t -> unit
val appi: (int * 'a -> unit) -> 'a t -> unit
val foldl: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
val all: ('a -> bool) -> 'a t -> bool
val alli: (int * 'a -> bool) -> 'a t -> bool
val exists: ('a -> bool) -> 'a t -> bool
val existsi: (int * 'a -> bool) -> 'a t -> bool
val find: ('a -> bool) -> 'a t -> 'a option

(* Transforming sequences *)
val map: ('a -> 'b) -> 'a t -> 'b t
val mapi: (int * 'a -> 'b) -> 'a t -> 'b t
val filter: ('a -> bool) -> 'a t -> 'a t
val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
val scan: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b t
val take: int -> 'a t -> 'a t
val drop: int -> 'a t -> 'a t
val takeWhile: ('a -> bool) -> 'a t -> 'a t
val dropWhile: ('a -> bool) -> 'a t -> 'a t
val groupBy: ('a * 'a -> bool) -> 'a t -> 'a t t
val memoize: 'a t -> 'a t
val zip: 'a t * 'b t -> ('a * 'b) t
val unzip: ('a * 'b) t -> 'a t * 'b t
val transpose: 'a t t -> 'a t t
val product: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t

end
(* vim: set tw=0 ts=3 sw=3: *)
