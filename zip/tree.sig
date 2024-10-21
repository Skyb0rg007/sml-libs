
signature TREE =
sig

type ptr
datatype node =
   Node of ptr * ptr
 | Leaf of int

val root: ptr
val := : ptr * node -> unit
val ! : ptr -> node
val parent: ptr -> ptr option

type 'a computation
val compute: 'a computation * ptr -> 'a
val discard: 'a computation -> unit

(* Bottom-up computation *)
datatype 'a nodeF =
   NodeF of 'a * 'a
 | LeafF of int

val foldUp: ('a nodeF -> 'a) -> 'a computation

(* Top-down computation *)
datatype nodeCtx =
   Root
 | Left of nodeCtx * ptr
 | Right of ptr * nodeCtx

(* val foldDown: (('a -> 'b) nodeF -> 'a -> 'b) -> 'a -> 'b computation *)

(* val foldDown: ('a nodeD -> 'a nodeF) -> 'a computation *)

end

(* vim: set tw=0 ts=3 sw=3: *)
