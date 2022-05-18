
structure Tree:
sig
   type 'a tree
   type 'a zipper

   datatype 'a node =
      Leaf of 'a
    | Node of 'a tree list

   val wrap: 'a node -> 'a tree
   val unwrap: 'a tree -> 'a node
   val leaf: 'a -> 'a tree
   val node: 'a tree list -> 'a tree
   val zipper: 'a tree -> 'a zipper

   val cur: 'a zipper -> 'a tree
   val modify: ('a tree -> 'a tree) -> 'a zipper -> 'a zipper
   val up: 'a zipper -> 'a zipper option
   val down: 'a zipper -> 'a zipper option
   val left: 'a zipper -> 'a zipper option
   val right: 'a zipper -> 'a zipper option
end =
struct
   datatype 'a node =
      Leaf of 'a
    | Node of 'a tree list
   withtype 'a tree = 'a node * PropList.holder

   datatype 'a dtree = DNode of 'a tree list * 'a tree list * PropList.holder

   type 'a zipper = 'a dtree list * 'a tree

   fun wrap n = (n, PropList.newHolder ())

   fun unwrap (n, _) = n

   fun leaf x = wrap (Leaf x)

   fun node ts = wrap (Node ts)

   fun zipper t = ([], t)

   fun cur (_, t) = t

   fun modify f (ds, t) = (ds, f t)

   fun up (DNode (l, r, h) :: ds, t) = SOME (ds, (Node (List.revAppend (l, t :: r)), h))
     | up _ = NONE

   fun down (ds, (Node (t :: ts), h)) = SOME (DNode ([], ts, h) :: ds, t)
     | down _ = NONE

   fun left (DNode (l::ls, r, h) :: ds, t) = SOME (DNode (ls, t::r, h) :: ds, l)
     | left _ = NONE

   fun right (DNode (l, r::rs, h) :: ds, t) = SOME (DNode (t::l, rs, h) :: ds, r)
     | right _ = NONE
end

structure RAG =
struct
   fun locmin z =
      case Tree.unwrap (Tree.cur z) of
         Tree.Leaf l => l
       | Tree.Node _ =>
            let
               val z' = Option.valOf (Tree.down z)
               fun go (z, acc) =
                  case Tree.right z of
                     NONE => acc
                   | SOME r => go (r, Int.min (locmin r, acc))
            in
               go (z', locmin z')
            end

   fun globmin z =
      case Tree.up z of
         SOME z' => globmin z'
       | NONE => locmin z

   fun replace z =
      case Tree.unwrap (Tree.cur z) of
         Tree.Leaf n => Tree.modify (fn _ => Tree.leaf (globmin z)) z
       | Tree.Node _ =>
            let
               fun go z =
                  case Tree.right z of
                     NONE => Option.valOf (Tree.up z)
                   | SOME r => go (replace r)
            in
               go (replace (Option.valOf (Tree.down z)))
            end

   val t = Tree.node [Tree.node [Tree.leaf 1], Tree.leaf 2, Tree.node [Tree.leaf 3, Tree.leaf 0]]
end

(* vim: set ts=3 sw=3 tw=0: *)
