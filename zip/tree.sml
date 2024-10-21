
structure Tree =
struct

type ptr = word

datatype node =
   Node of ptr * ptr
 | Leaf of int

exception NotFound

val tree: node WordHashTable.hash_table = WordHashTable.mkTable (32, NotFound)

val () = WordHashTable.insert tree (0w0, Leaf 0)

val root = 0w0

val ! = WordHashTable.lookup tree
val op := = WordHashTable.insert tree

fun parent _ = raise Fail "NYI"

end

(* vim: set tw=0 ts=3 sw=3: *)
