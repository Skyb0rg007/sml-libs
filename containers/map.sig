
signature MAP =
sig

type 'a map
type key

(** Construction **)
val empty: 'a map
val singleton: key * 'a -> 'a map
val fromList: (key * 'a) list -> 'a map
val fromListWith: ('a * 'a -> 'a) -> (key * 'a) list -> 'a map
val fromListWithi: (key * 'a * 'a -> 'a) -> (key * 'a) list -> 'a map

(** Insertion **)
val insert: 'a map * key * 'a -> 'a map
val insertWith: ('a * 'a -> 'a) -> 'a map * key * 'a -> 'a map
val insertWithi: (key * 'a * 'a -> 'a) -> 'a map * key * 'a -> 'a map
val insert': (key * 'a) * 'a map -> 'a map
val insertLookupWithi: (key * 'a * 'a -> 'a) -> 'a map * key * 'a -> 'a option * 'a map

(** Deletion/Update **)
val delete: 'a map * key -> 'a map
val remove: 'a map * key -> ('a map * 'a) option
val adjust: ('a -> 'a) -> 'a map * key -> 'a map
val update: ('a -> 'a option) -> 'a map * key -> 'a map
val updateLookupWithi: (key * 'a -> 'a option) -> 'a map * key -> 'a option * 'a map
val alter: ('a option -> 'a option) -> 'a map * key -> 'a map

(** Lookup **)
val find: 'a map * key -> 'a option
val inDomain: 'a map * key -> bool

(** Size **)
val isEmpty: 'a map -> bool
val size: 'a map -> int

(** Union **)
val unionWith: ('a * 'a -> 'a) -> 'a map * 'a map -> 'a map
val unionWithi: (key * 'a * 'a -> 'a) -> 'a map * 'a map -> 'a map

(** Difference **)
val difference: 'a map * 'b map -> 'a map
val differenceWith: ('a * 'b -> 'a option) -> 'a map * 'b map -> 'a map
val differenceWithi: (key * 'a * 'b -> 'a option) -> 'a map * 'b map -> 'a map

(** Intersection **)
val intersection: 'a map * 'b map -> 'a map
val intersectionWith: ('a * 'b -> 'c option) -> 'a map * 'b map -> 'c map
val intersectionWithi: (key * 'a * 'b -> 'c option) -> 'a map * 'b map -> 'c map

(** Disjoint **)
val disjoint: 'a map * 'b map -> bool

(** Traversal **)
val map: ('a -> 'b) -> 'a map -> 'b map
val mapi: (key * 'a -> 'b) -> 'a map -> 'b map
val mapAccumL: ('a * 'b -> 'b * 'c) -> 'b -> 'a map -> 'b * 'c map
val mapAccumLi: (key * 'a * 'b -> 'b * 'c) -> 'b -> 'a map -> 'b * 'c map
val mapAccumR: ('a * 'b -> 'b * 'c) -> 'b -> 'a map -> 'b * 'c map
val mapAccumRi: (key * 'a * 'b -> 'b * 'c) -> 'b -> 'a map -> 'b * 'c map
val foldl: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
val foldli: (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
val foldr: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
val foldri: (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
val app: ('a -> unit) -> 'a map -> unit
val appi: (key * 'a -> unit) -> 'a map -> unit
val exists: ('a -> bool) -> 'a map -> bool
val existsi: (key * 'a -> bool) -> 'a map -> bool
val all: ('a -> bool) -> 'a map -> bool
val alli: (key * 'a -> bool) -> 'a map -> bool

(** Conversion **)
val keys: 'a map -> key list
val elems: 'a map -> 'a list
val toList: 'a map -> (key * 'a) list

(** Filter **)
val filter: ('a -> bool) -> 'a map -> 'a map
val filteri: (key * 'a -> bool) -> 'a map -> 'a map
val mapPartial: ('a -> 'b option) -> 'a map -> 'b map
val mapPartiali: (key * 'a -> 'b option) -> 'a map -> 'b map
val mapEither: ('a -> ('b, 'c) Either.either) -> 'a map -> 'b map * 'c map
val mapEitheri: (key * 'a -> ('b, 'c) Either.either) -> 'a map -> 'b map * 'c map
val partition: ('a -> bool) -> 'a map -> 'a map * 'a map
val partitioni: (key * 'a -> bool) -> 'a map -> 'a map * 'a map

(** Submap **)
val isSubmapBy: ('a * 'b -> bool) -> 'a map * 'b map -> bool
val isProperSubmapBy: ('a * 'b -> bool) -> 'a map * 'b map -> bool

(** Comparisons **)
val liftEquals: ('a * 'b -> bool) -> 'a map * 'b map -> bool
val collate: ('a * 'b -> order) -> 'a map * 'b map -> order

end

(* vim: set tw=0 ts=3 sw=3: *)
