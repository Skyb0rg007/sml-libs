
signature MONO_MAP =
sig

type map
type key
type value

(** Construction **)
val empty: map
val singleton: key * value -> map
val fromList: (key * value) list -> map
val fromListWith: (value * value -> value) -> (key * value) list -> map
val fromListWithi: (key * value * value -> value) -> (key * value) list -> map

(** Insertion **)
val insert: map * key * value -> map
val insertWith: (value * value -> value) -> map * key * value -> map
val insertWithi: (key * value * value -> value) -> map * key * value -> map
val insert': (key * value) * map -> map
val insertLookupWithi: (key * value * value -> value) -> map * key * value -> value option * map

(** Deletion/Update **)
val delete: map * key -> map
val remove: map * key -> (map * value) option
val adjust: (value -> value) -> map * key -> map
val update: (value -> value option) -> map * key -> map
val updateLookupWithi: (key * value -> value option) -> map * key -> value option * map
val alter: (value option -> value option) -> map * key -> map

(** Lookup **)
val find: map * key -> value option
val inDomain: map * key -> bool

(** Size **)
val isEmpty: map -> bool
val size: map -> int

(** Union **)
val unionWith: (value * value -> value) -> map * map -> map
val unionWithi: (key * value * value -> value) -> map * map -> map

(** Difference **)
val difference: map * map -> map
val differenceWith: (value * value -> value option) -> map * map -> map
val differenceWithi: (key * value * value -> value option) -> map * map -> map

(** Intersection **)
val intersection: map * map -> map
val intersectionWith: (value * value -> value option) -> map * map -> map
val intersectionWithi: (key * value * value -> value option) -> map * map -> map

(** Disjoint **)
val disjoint: map * map -> bool

(** Traversal **)
val map: (value -> value) -> map -> map
val mapi: (key * value -> value) -> map -> map
val mapAccumL: (value * 'b -> 'b * value) -> 'b -> map -> 'b * map
val mapAccumLi: (key * value * 'b -> 'b * value) -> 'b -> map -> 'b * map
val mapAccumR: (value * 'b -> 'b * value) -> 'b -> map -> 'b * map
val mapAccumRi: (key * value * 'b -> 'b * value) -> 'b -> map -> 'b * map
val foldl: (value * 'b -> 'b) -> 'b -> map -> 'b
val foldli: (key * value * 'b -> 'b) -> 'b -> map -> 'b
val foldr: (value * 'b -> 'b) -> 'b -> map -> 'b
val foldri: (key * value * 'b -> 'b) -> 'b -> map -> 'b
val app: (value -> unit) -> map -> unit
val appi: (key * value -> unit) -> map -> unit
val exists: (value -> bool) -> map -> bool
val existsi: (key * value -> bool) -> map -> bool
val all: (value -> bool) -> map -> bool
val alli: (key * value -> bool) -> map -> bool

(** Conversion **)
val keys: map -> key list
val elems: map -> value list
val toList: map -> (key * value) list

(** Filter **)
val filter: (value -> bool) -> map -> map
val filteri: (key * value -> bool) -> map -> map
val mapPartial: (value -> value option) -> map -> map
val mapPartiali: (key * value -> value option) -> map -> map
val mapEither: (value -> (value, value) Either.either) -> map -> map * map
val mapEitheri: (key * value -> (value, value) Either.either) -> map -> map * map
val partition: (value -> bool) -> map -> map * map
val partitioni: (key * value -> bool) -> map -> map * map

(** Submap **)
val isSubmapBy: (value * value -> bool) -> map * map -> bool
val isProperSubmapBy: (value * value -> bool) -> map * map -> bool

(** Comparisons **)
val liftEquals: (value * value -> bool) -> map * map -> bool
val collate: (value * value -> order) -> map * map -> order

end

(* vim: set tw=0 ts=3 sw=3: *)
