
structure AG =
struct
   structure Susp = SMLofNJ.Susp

   structure Tree =
   struct
      datatype 'a t =
         Leaf of PropList.holder * 'a
       | Node of PropList.holder * 'a t * 'a t

      fun leaf x = Leaf (PropList.newHolder (), x)
      fun node (l, r) = Node (PropList.newHolder (), l, r)

      fun toString s (Leaf (_, x)) = "Leaf " ^ s x
        | toString s (Node (_, l, r)) = "Node (" ^ toString s l ^ ", " ^ toString s r ^ ")"

      fun holder (Leaf (p, _)) = p
        | holder (Node (p, _, _)) = p

      fun map f (Leaf (p, x)) = Leaf (p, f x)
        | map f (Node (p, l, r)) = Node (p, map f l, map f r)

      fun foldr f z (Leaf (_, x)) = f (x, z)
        | foldr f z (Node (_, l, r)) = foldr f (foldr f z r) l

      fun foldl f z (Leaf (_, x)) = f (x, z)
        | foldl f z (Node (_, l, r)) = foldl f (foldl f z l) r
   end

   structure Zip:
   sig
      type 'a t

      datatype 'a cons =
         CRoot of 'a t
       | CNode of PropList.holder * 'a t * 'a t
       | CLeaf of PropList.holder * 'a

      exception Invalid

      val match: 'a t -> 'a cons
      val up: 'a t -> 'a t
      val modify: ('a Tree.t -> 'a Tree.t) -> 'a t -> 'a t
      val fromTree: 'a Tree.t -> 'a t
      val holder: 'a t -> PropList.holder

      val toString: ('a -> string) -> 'a t -> string
   end =
   struct
      datatype 'a ctx =
         Root
       | Top
       | Left of PropList.holder * 'a ctx * 'a Tree.t
       | Right of PropList.holder * 'a Tree.t * 'a ctx

      type 'a t = 'a ctx * 'a Tree.t

      datatype 'a cons =
         CRoot of 'a t
       | CNode of PropList.holder * 'a t * 'a t
       | CLeaf of PropList.holder * 'a

      exception Invalid

      fun holder (_, t) = Tree.holder t

      fun ctxString s Root = "Root"
        | ctxString s Top = "Top"
        | ctxString s (Left (_, ctx, t)) = "Left (" ^ ctxString s ctx ^ ", " ^ Tree.toString s t ^ ")"
        | ctxString s (Right (_, t, ctx)) = "Right (" ^ Tree.toString s t ^ ", " ^ ctxString s ctx ^ ")"

      fun toString s (c, t) = "(" ^ ctxString s c ^ ", " ^ Tree.toString s t ^ ")"

      fun fromTree t = (Root, t)

      fun match (Root, t) = CRoot (Top, t)
        | match (_, Tree.Leaf (p, x)) = CLeaf (p, x)
        | match (c, Tree.Node (p, l, r)) = CNode (p, (Left (p, c, r), l), (Right (p, l, c), r))

      fun up (Top, t) = (Root, t)
        | up (Left (p, c, r), t) = (c, Tree.Node (p, t, r))
        | up (Right (p, l, c), t) = (c, Tree.Node (p, l, t))
        | up _ = raise Invalid

      fun modify f (c, t) = (c, f t)
   end

   val {peekFn = getGlobmin: int Zip.t -> int option, setFn = setGlobmin, ...} = PropList.newProp (Zip.holder, fn _ => raise Option)
   val {peekFn = getLocmin: int Zip.t -> int option, setFn = setLocmin, ...} = PropList.newProp (Zip.holder, fn _ => raise Option)
   val {peekFn = getReplace: int Zip.t -> int Tree.t option, setFn = setReplace, ...} = PropList.newProp (Zip.holder, fn _ => raise Option)

   fun globmin' z =
      case Zip.match z of
         Zip.CRoot tree => locmin tree
       | Zip.CLeaf _ => globmin (Zip.up z)
       | Zip.CNode _ => globmin (Zip.up z)

   and locmin' z =
      case Zip.match z of
         Zip.CLeaf (_, n) => n
       | Zip.CNode (_, l, r) => Int.min (locmin l, locmin r)
       | _ => raise Fail "locmin: Got Root"

   and globmin z =
      case getGlobmin z of
         SOME x => x
       | NONE =>
            let
               val x = globmin' z
            in
               TextIO.print "globmin\n"
               ; TextIO.print (Zip.toString Int.toString z ^ "\n")
               ; TextIO.print (" -> " ^ Int.toString x ^ "\n")
               ; setGlobmin (z, x)
               ; x
            end

   and locmin z =
      case getLocmin z of
         SOME x => x
       | NONE =>
            let
               val x = locmin' z
            in
               TextIO.print "locmin\n"
               ; TextIO.print (Zip.toString Int.toString z ^ "\n")
               ; TextIO.print (" -> " ^ Int.toString x ^ "\n")
               ; setLocmin (z, x)
               ; x
            end

   fun replace' z =
      case Zip.match z of
         Zip.CRoot tree => replace tree
       | Zip.CLeaf (p, _) => Tree.Leaf (p, globmin z)
       | Zip.CNode (p, l, r) => Tree.Node (p, replace l, replace r)

   and replace z =
      case getReplace z of
         SOME x => x
       | NONE =>
            let
               val x = replace' z
            in
               setReplace (z, x); x
            end
end

(* vim: set ts=3 sw=3 tw=0: *)
