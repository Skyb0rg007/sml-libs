
signature EQ_KEY =
sig
   type eq_key

   val sameKey: eq_key * eq_key -> bool
end

signature EQ_MAP =
sig
   type 'a map

   structure Key: EQ_KEY

   (* Creation *)
   val empty: 'a map
   val singleton: Key.eq_key * 'a -> 'a map
   val fromListWithi: (Key.eq_key * 'a * 'a -> 'a) -> (Key.eq_key * 'a) list -> 'a map
   val fromListWith: ('a * 'a -> 'a) -> (Key.eq_key * 'a) list -> 'a map
   val fromList: (Key.eq_key * 'a) list -> 'a map
   val fromDistinctList: (Key.eq_key * 'a) list -> 'a map

   (* Insertion *)
   val insert: 'a map * Key.eq_key * 'a -> 'a map
   val insert': (Key.eq_key * 'a) * 'a map -> 'a map
   val insertWith: ('a * 'a -> 'a) -> 'a map * Key.eq_key * 'a -> 'a map
   val insertWithi: (Key.eq_key * 'a * 'a -> 'a) -> 'a map * Key.eq_key * 'a -> 'a map
   val insertLookupWithi: (Key.eq_key * 'a * 'a -> 'a) -> 'a map * Key.eq_key * 'a -> 'a map * 'a option

   (* Lookup *)
   val find: 'a map * Key.eq_key -> 'a option
   val lookup: 'a map * Key.eq_key -> 'a
   val inDomain: 'a map * Key.eq_key -> bool

   (* Queries *)
   val size: 'a map -> int
   val isEmpty: 'a map -> bool

   (* Combining *)
   val union: 'a map * 'a map -> 'a map
   val unionWith: ('a * 'a -> 'a) -> 'a map * 'a map -> 'a map
   val unionWithi: (Key.eq_key * 'a * 'a -> 'a) -> 'a map * 'a map -> 'a map

   (* Deletion *)
   val remove: 'a map * Key.eq_key -> ('a map * 'a) option
   val delete: 'a map * Key.eq_key -> 'a map

   (* Higher-order functions *)
   val map: ('a -> 'b) -> 'a map -> 'b map
   val mapi: (Key.eq_key * 'a -> 'b) -> 'a map -> 'b map
   val mapPartial: ('a -> 'b option) -> 'a map -> 'b map
   val mapPartiali: (Key.eq_key * 'a -> 'b option) -> 'a map -> 'b map
   val foldl: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
   val foldli: (Key.eq_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
   val foldr: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
   val foldri: (Key.eq_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
   val all: ('a -> bool) -> 'a map -> bool
   val alli: (Key.eq_key * 'a -> bool) -> 'a map -> bool
   val exists: ('a -> bool) -> 'a map -> bool
   val existsi: (Key.eq_key * 'a -> bool) -> 'a map -> bool

   (* Conversion *)
   val listItems: 'a map -> 'a list
   val listItemsi: 'a map -> (Key.eq_key * 'a) list
   val listKeys: 'a map -> Key.eq_key list

   (* Submap *)
   val isSubmapBy: ('a * 'b -> bool) -> 'a map * 'b map -> bool
   val isProperSubmapBy: ('a * 'b -> bool) -> 'a map * 'b map -> bool

   (* Comparison *)
   val same: ('a * 'a -> bool) -> 'a map * 'a map -> bool
   val collate: (Key.eq_key * Key.eq_key -> order)
             -> ('a * 'b -> order)
             -> 'a map * 'b map
             -> order
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
