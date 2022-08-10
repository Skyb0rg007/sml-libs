
signature LAZY_SHARING_WORD_MAP_STRUCTS =
sig
   type t

   val same: t * t -> bool
end

signature LAZY_SHARING_WORD_MAP =
sig
   type key = word
   type value

   type t

   val empty: t
   val singleton: key * value -> t
   val fromList: (key * value) list -> t

   val find: t * key -> value option

   val insert: t * key * value -> t
   val insert': (key * value) * t -> t
   val insertWith: (value * value -> value) -> t * key * value -> t
   val insertWithi: (key * value * value -> value) -> t * key * value -> t

   val union: t * t -> t
   val unionWith: (value * value -> value) -> t * t -> t
   val unionWithi: (key * value * value -> value) -> t * t -> t

   val foldli: (key * value * 'a -> 'a) -> 'a -> t -> 'a
   val foldl: (value * 'a -> 'a) -> 'a -> t -> 'a
   val foldri: (key * value * 'a -> 'a) -> 'a -> t -> 'a
   val foldr: (value * 'a -> 'a) -> 'a -> t -> 'a
   val mapi: (key * value -> value) -> t -> t
   val map: (value -> value) -> t -> t
   val toList: t -> (key * value) list

   val same: t * t -> bool
end

(* vim: set tw=0 ts=3 sw=3: *)
