
signature SPARSE_VECTOR =
sig
   type 'a t

   (* Construction *)
   val empty: 'a t
   val singleton: int * 'a -> 'a t
   val fromList: (int * 'a) list -> 'a t
   val fromListWith: ('a * 'a -> 'a) -> (int * 'a) list -> 'a t
   val fromListWithi: (int * 'a * 'a -> 'a) -> (int * 'a) list -> 'a t

   (* Insertion *)
   val insert: 'a t * int * 'a -> 'a t
   val insertWith: ('a * 'a -> 'a) -> 'a t * int * 'a -> 'a t
   val insertWithi: (int * 'a * 'a -> 'a) -> 'a t * int * 'a -> 'a t
   val insertLookupWithi: (int * 'a * 'a -> 'a) -> 'a t * int * 'a -> 'a t * 'a option

   (* Removal *)
   val remove: 'a t * int -> ('a t * 'a) option
   val delete: 'a t * int -> 'a t

   (* Updating *)
   val adjust: ('a -> 'a) -> 'a t * int -> 'a t
   val adjusti: (int * 'a -> 'a) -> 'a t * int -> 'a t
   val update: ('a -> 'a option) -> 'a t * int -> 'a t
   val updatei: (int * 'a -> 'a option) -> 'a t * int -> 'a t
   val updateLookupi: (int * 'a -> 'a option) -> 'a t * int -> 'a t * 'a option
   val alter: ('a option -> 'a option) -> 'a t * int -> 'a t
   val alteri: (int * 'a option -> 'a option) -> 'a t * int -> 'a t

   (* Lookup *)
   val find: 'a t * int -> 'a option
   val lookup: 'a t * int -> 'a
   val inDomain: 'a t * int -> bool
   (* val findLT: 'a t * int -> (int * 'a) option *)
   (* val findGT: 'a t * int -> (int * 'a) option *)
   (* val findLE: 'a t * int -> (int * 'a) option *)
   (* val findGE: 'a t * int -> (int * 'a) option *)

   (* Queries *)
   val isEmpty: 'a t -> bool
   val length: 'a t -> int
   val maxLen: int

   (* Combining *)
   val union: 'a t * 'a t -> 'a t
   val unionWith: ('a * 'a -> 'a) -> 'a t * 'a t -> 'a t
   val unionWithi: (int * 'a * 'a -> 'a) -> 'a t * 'a t -> 'a t

   (* Difference *)
   (* val difference: 'a t * 'b t -> 'a t *)
   (* val differenceWith: ('a * 'b -> 'a option) -> 'a t * 'b t -> 'a t *)
   (* val differenceWithi: (int * 'a * 'b -> 'a option) -> 'a t * 'b t -> 'a t *)

   (* Intersection *)
   (* val intersection: 'a t * 'b t -> 'a t *)
   (* val intersectionWith: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t *)
   (* val intersectionWithi: (int * 'a * 'b -> 'c) -> 'a t * 'b t -> 'c t *)

   (* Disjoint *)
   val disjoint: 'a t * 'b t -> bool

   (* Higher-order operators *)
   val map: ('a -> 'b) -> 'a t -> 'b t
   val mapi: (int * 'a -> 'b) -> 'a t -> 'b t
   val mapAccuml: ('a * 'c -> 'b * 'c) -> 'c -> 'a t -> 'c * 'b t
   (* val mapAccumli: (int * 'c * 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t *)
   (* val mapAccumr: ('c * 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t *)
   (* val mapAccumri: (int * 'c * 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t *)
   val mapPartial: ('a -> 'b option) -> 'a t -> 'b t
   val mapPartiali: (int * 'a -> 'b option) -> 'a t -> 'b t
   val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldl: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldr: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val all: ('a -> bool) -> 'a t -> bool
   val alli: (int * 'a -> bool) -> 'a t -> bool
   val exists: ('a -> bool) -> 'a t -> bool
   val existsi: (int * 'a -> bool) -> 'a t -> bool

   (* Conversion *)
   val listItems: 'a t -> 'a list
   val listItemsi: 'a t -> (int * 'a) list
   val listKeys: 'a t -> int list

   (* Submap *)
   val isSubmapBy: ('a * 'b -> bool) -> 'a t * 'b t -> bool
   val isProperSubmapBy: ('a * 'b -> bool) -> 'a t * 'b t -> bool

   (* Min/Max *)
   val findMin: 'a t -> (int * 'a) option
   val findMax: 'a t -> (int * 'a) option
   (* raises Fail on empty *)
   val lookupMin: 'a t -> int * 'a
   val lookupMax: 'a t -> int * 'a
   (* no-ops on empty *)
   val deleteMin: 'a t -> 'a t
   val deleteMax: 'a t -> 'a t

   (* Comparisons *)
   val same: ('a * 'b -> bool) -> 'a t * 'b t -> bool
   val collate: ('a * 'b -> order) -> 'a t * 'b t -> order
end

(* vim: set ft=sml tw=0 sw=3 ts=3: *)
