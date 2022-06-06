
(* 2-3 tree node *)
functor NodeFn(M: MONOID):
   sig
      datatype 'a t =
         Node2 of M.t * 'a * 'a
       | Node3 of M.t * 'a * 'a * 'a

      val node2: ('a -> M.t) -> 'a * 'a -> 'a t
      val node3: ('a -> M.t) -> 'a * 'a * 'a -> 'a t

      val toDigit: 'a t -> 'a Digit.t
      val measure: 'a t -> M.t
      val map: ('a -> 'b) -> 'a t -> 'b t
      val foldl: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
      val foldr: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
      val equals: ('a * 'b -> bool) -> 'a t * 'b t -> bool
      val compare: ('a * 'b -> order) -> 'a t * 'b t -> order
      val toString: (M.t -> string) -> ('a -> string) -> 'a t -> string
   end =
   struct
      datatype 'a t =
         Node2 of M.t * 'a * 'a
       | Node3 of M.t * 'a * 'a * 'a

      fun node2 f (x, y) = Node2 (M.+ (f x, f y), x, y)

      fun node3 f (x, y, z) = Node3 (M.+ (M.+ (f x, f y), f z), x, y, z)

      fun toDigit (Node2 (_, x, y)) = Digit.Two (x, y)
        | toDigit (Node3 (_, x, y, z)) = Digit.Three (x, y, z)

      fun measure (Node2 (m, _, _)) = m
        | measure (Node3 (m, _, _, _)) = m

      fun map f (Node2 (m, x, y)) = Node2 (m, f x, f y)
        | map f (Node3 (m, x, y, z)) = Node3 (m, f x, f y, f z)

      fun foldl f acc (Node2 (_, x, y)) = f (y, f (x, acc))
        | foldl f acc (Node3 (_, x, y, z)) = f (z, f (y, f (x, acc)))

      fun foldr f acc (Node2 (_, x, y)) = f (x, f (y, acc))
        | foldr f acc (Node3 (_, x, y, z)) = f (x, f (y, f (z, acc)))

      fun equals eq (Node2 (_, x, y), Node2 (_, x', y')) =
         eq (x, x') andalso eq (y, y')
        | equals eq (Node3 (_, x, y, z), Node3 (_, x', y', z')) =
         eq (x, x') andalso eq (y, y') andalso eq (z, z')
        | equals _ _ = false

      fun compare cmp (Node2 (_, x, y), Node2 (_, x', y')) =
         (case cmp (x, x') of
             EQUAL => cmp (y, y')
           | order => order)
        | compare cmp (Node3 (_, x, y, z), Node3 (_, x', y', z')) =
         (case cmp (x, x') of
             EQUAL =>
                (case cmp (y, y') of
                    EQUAL => cmp (z, z')
                  | order => order)
           | order => order)
        | compare cmp (Node2 _, Node3 _) = LESS
        | compare cmp (Node3 _, Node2 _) = GREATER

      fun toString tos tos' (Node2 (m, x, y)) = String.concat
         ["Node2 (", tos m, ", ", tos' x, ", ", tos' y, ")"]
        | toString tos tos' (Node3 (m, x, y, z)) = String.concat
         ["Node3 (", tos m, ", ", tos' x, ", ", tos' y, ", ", tos' z, ")"]
   end

functor FingerTree(S: FINGER_TREE_STRUCTS) :> FINGER_TREE
   where type Measure.t = S.Measure.t
     and type 'a Item.t = 'a S.Item.t =
   struct
      open S

      infix 6 <+>
      val op <+> = Measure.+

      structure Susp = SMLofNJ.Susp

      structure Node = NodeFn(Measure)

      (* `'a Elem.t` is `'a Item.t` or `'a Elem.t Node.t`,
       * Standard definition uses polymorphic recursion to enforce
       * the fact that each item is on the same level
       * This is just an invariant for the SML implementation *)
      structure Elem =
         struct
            datatype 'a t =
               Item of 'a Item.t
             | Node of 'a t Node.t

            fun measure (Item item) = Item.measure item
              | measure (Node node) = Node.measure node

            fun toItem (Item item) = item
              | toItem _ = raise Fail "FingerTree.Elem.toItem: invariant broken"

            fun toNode (Node node) = node
              | toNode _ = raise Fail "FingerTree.Elem.toNode: invariant broken"

            fun node2 args = Node (Node.node2 measure args)

            fun node3 args = Node (Node.node3 measure args)

            fun toString tos tos' =
               let
                  fun go (Item item) = tos' item
                    | go (Node node) = Node.toString tos go node
               in
                  go
               end
         end

      datatype 'a t =
         Empty
       | Single of 'a Elem.t
       | Deep of {left: 'a Elem.t Digit.t,
                  measure: Measure.t Lazy.t,
                  right: 'a Elem.t Digit.t,
                  subtree: 'a t Lazy.t}

      fun debugShow _ _ Empty = "Empty"
        | debugShow tos tos' (Single x) = String.concat ["Single ", Elem.toString tos tos' x]
        | debugShow tos tos' (Deep {left, measure, right, subtree}) =
         String.concat ["Deep {measure = ",
                        tos (Lazy.force measure),
                        ", left = ",
                        Digit.toString (Elem.toString tos tos') left,
                        ", right = ",
                        Digit.toString (Elem.toString tos tos') right,
                        ", subtree = ",
                        debugShow tos tos' (Lazy.force subtree),
                        "}" ]

      fun measureDigit d = Digit.foldMap Measure.+ Elem.measure d

      fun force (Deep {measure, ...}) = ignore (Lazy.force measure)
        | force _ = ()

      fun measure Empty = Measure.zero
        | measure (Single x) = Elem.measure x
        | measure (Deep {measure, ...}) = Lazy.force measure

      val empty = Empty

      fun singleton x = Single (Elem.Item x)

      fun isEmpty Empty = true
        | isEmpty _ = false

      fun deep (left, subtree, right) =
         Deep {left = left,
               right = right,
               subtree = subtree,
               measure = Lazy.delay (fn () =>
                  measureDigit left
                  <+> measure (Lazy.force subtree)
                  <+> measureDigit right)
         }

      val _: 'a Elem.t Digit.t * 'a t Lazy.t * 'a Elem.t Digit.t -> 'a t = deep

      fun fromDigit (Digit.One x) = Single x
        | fromDigit (Digit.Two (x, y)) = deep (Digit.One x, Lazy.eager Empty, Digit.One y)
        | fromDigit (Digit.Three (x, y, z)) = deep (Digit.Two (x, y), Lazy.eager Empty, Digit.One z)
        | fromDigit (Digit.Four (x, y, z, w)) = deep (Digit.Two (x, y), Lazy.eager Empty, Digit.Two (z, w))

      val _: 'a Elem.t Digit.t -> 'a t = fromDigit

      local
         fun foldlTree _ acc Empty = acc
           | foldlTree f acc (Single x) = f (x, acc)
           | foldlTree f acc (Deep {left, right, subtree, ...}) =
            Digit.foldl f
               (foldlTree 
                  (fn (elem, acc) => Node.foldl f acc (Elem.toNode elem))
                  (Digit.foldl f acc left)
                  (Lazy.force subtree))
               right

         val _: ('a Elem.t * 'b -> 'b) -> 'b -> 'a t -> 'b = foldlTree
      in
         fun foldl f = foldlTree (fn (elem, acc) => f (Elem.toItem elem, acc))
      end

      local
         fun foldrTree _ acc Empty = acc
           | foldrTree f acc (Single x) = f (x, acc)
           | foldrTree f acc (Deep {left, right, subtree, ...}) =
            Digit.foldr f
               (foldrTree 
                  (fn (elem, acc) => Node.foldr f acc (Elem.toNode elem))
                  (Digit.foldr f acc right)
                  (Lazy.force subtree))
               left

         val _: ('a Elem.t * 'b -> 'b) -> 'b -> 'a t -> 'b = foldrTree
      in
         fun foldr f = foldrTree (fn (elem, acc) => f (Elem.toItem elem, acc))
      end

      fun toList t = foldr op :: [] t

      fun consTree (elem, Empty) = Single elem
        | consTree (elem, Single x) =
         deep (Digit.One elem, Lazy.eager Empty, Digit.One x)
        | consTree (elem, Deep {left, right, subtree, ...}) =
         case left of
            Digit.One x => deep (Digit.Two (elem, x), subtree, right)
          | Digit.Two (x, y) => deep (Digit.Three (elem, x, y), subtree, right)
          | Digit.Three (x, y, z) => deep (Digit.Four (elem, x, y, z), subtree, right)
          | Digit.Four (x, y, z, w) =>
               deep (Digit.Two (elem, x),
                     Lazy.eager (consTree (Elem.node3 (y, z, w), Lazy.force subtree)),
                     right)

      fun snocTree (Empty, elem) = Single elem
        | snocTree (Single x, elem) =
         deep (Digit.One x, Lazy.eager Empty, Digit.One elem)
        | snocTree (Deep {left, right, subtree, ...}, elem) =
         case right of
            Digit.One x => deep (left, subtree, Digit.Two (x, elem))
          | Digit.Two (x, y) => deep (left, subtree, Digit.Three (x, y, elem))
          | Digit.Three (x, y, z) => deep (left, subtree, Digit.Four (x, y, z, elem))
          | Digit.Four (x, y, z, w) =>
               deep (left,
                     Lazy.eager (snocTree (Lazy.force subtree, Elem.node3 (x, y, z))),
                     Digit.Two (w, elem))

      val _: 'a Elem.t * 'a t -> 'a t = consTree
      val _: 'a t * 'a Elem.t -> 'a t = snocTree

      fun cons (x, t) = consTree (Elem.Item x, t)

      fun snoc (t, x) = snocTree (t, Elem.Item x)

      fun fromList xs = List.foldr cons empty xs

      fun viewlTree Empty = NONE
        | viewlTree (Single x) = SOME (x, Lazy.delay (fn () => Empty))
        | viewlTree (Deep {left, right, subtree, ...}) =
         case left of
            Digit.Two (x, y) => SOME (x, Lazy.delay (fn () => deep (Digit.One y, subtree, right)))
          | Digit.Three (x, y, z) => SOME (x, Lazy.delay (fn () => deep (Digit.Two (y, z), subtree, right)))
          | Digit.Four (x, y, z, w) => SOME (x, Lazy.delay (fn () => deep (Digit.Three (y, z, w), subtree, right)))
          | Digit.One x => SOME (x, Lazy.delay (fn () => rotL (Lazy.force subtree, right)))

      and rotL (subtree, right) =
         case viewlTree subtree of
            NONE => fromDigit right
          | SOME (a, subtree') => Deep {
               left = Node.toDigit (Elem.toNode a),
               right = right,
               subtree = subtree',
               measure = Lazy.delay (fn () =>
                  measure subtree <+> measureDigit right)
            }
   
      fun viewl t =
         case viewlTree t of
            NONE => NONE
          | SOME (x, t') => SOME (Elem.toItem x, t')

      fun viewrTree Empty = NONE
        | viewrTree (Single x) = SOME (Lazy.eager Empty, x)
        | viewrTree (Deep {left, right, subtree, ...}) =
         case right of
            Digit.Two (x, y) => SOME (Lazy.delay (fn () => deep (left, subtree, Digit.One x)), y)
          | Digit.Three (x, y, z) => SOME (Lazy.delay (fn () => deep (left, subtree, Digit.Two (x, y))), z)
          | Digit.Four (x, y, z, w) => SOME (Lazy.delay (fn () => deep (left, subtree, Digit.Three (x, y, z))), w)
          | Digit.One x =>
               SOME (Lazy.delay (fn () =>
                        case viewrTree (Lazy.force subtree) of
                           NONE => fromDigit left
                         | SOME (subtree', x') => 
                              deep (left, subtree', Node.toDigit (Elem.toNode x'))),
                     x)

      and rotR (left, subtree) =
         case viewrTree subtree of
            NONE => fromDigit left
          | SOME (subtree', a) => Deep {
               left = left,
               right = Node.toDigit (Elem.toNode a),
               subtree = subtree',
               measure = Lazy.delay (fn () =>
                  measureDigit left <+> measure subtree)
            }
      
      fun viewr t =
         case viewrTree t of
            NONE => NONE
          | SOME (t', x) => SOME (t', Elem.toItem x)

      (* Append *)

      (* `addDigitsN (t1, d1, ..., d2, t2)`
       * Append the arguments in order
       * t1 and t2 are subtrees,
       * d1 and d2 are digits,
       * the ... contains N `Elem.t`s *)

      (* `appTreeN (t1, ..., t2)`
       * Append the arguments in order
       * t1 and t2 are subtrees,
       * the ... contains N `Elem.t`s
       *)

      fun addDigits1 (t1, d1, x, d2, t2) =
         case (d1, x, d2) of
            (Digit.One a, b, Digit.One c) => appTree1 (t1, Elem.node3 (a, b, c), t2)
          | (Digit.One a, b, Digit.Two (c, d)) => appTree2 (t1, Elem.node2 (a, b), Elem.node2 (c, d), t2)
          | (Digit.One a, b, Digit.Three (c, d, e)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.One a, b, Digit.Four (c, d, e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)

          | (Digit.Two (a, b), c, Digit.One d) => appTree2 (t1, Elem.node2 (a, b), Elem.node2 (c, d), t2)
          | (Digit.Two (a, b), c, Digit.Two (d, e)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.Two (a, b), c, Digit.Three (d, e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Two (a, b), c, Digit.Four (d, e, f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)

          | (Digit.Three (a, b, c), d, Digit.One e) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.Three (a, b, c), d, Digit.Two (e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Three (a, b, c), d, Digit.Three (e, f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Three (a, b, c), d, Digit.Four (e, f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)

          | (Digit.Four (a, b, c, d), e, Digit.One f) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Four (a, b, c, d), e, Digit.Two (f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Four (a, b, c, d), e, Digit.Three (f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Four (a, b, c, d), e, Digit.Four (f, g, h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)

      and addDigits2 (t1, d1, x, y, d2, t2) =
         case (d1, x, y, d2) of
            (Digit.One a, b, c, Digit.One d) => appTree2 (t1, Elem.node2 (a, b), Elem.node2 (c, d), t2)
          | (Digit.One a, b, c, Digit.Two (d, e)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.One a, b, c, Digit.Three (d, e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.One a, b, c, Digit.Four (d, e, f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)

          | (Digit.Two (a, b), c, d, Digit.One e) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.Two (a, b), c, d, Digit.Two (e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Two (a, b), c, d, Digit.Three (e, f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Two (a, b), c, d, Digit.Four (e, f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)

          | (Digit.Three (a, b, c), d, e, Digit.One f) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Three (a, b, c), d, e, Digit.Two (f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Three (a, b, c), d, e, Digit.Three (f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Three (a, b, c), d, e, Digit.Four (f, g, h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)

          | (Digit.Four (a, b, c, d), e, f, Digit.One g) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Four (a, b, c, d), e, f, Digit.Two (g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Four (a, b, c, d), e, f, Digit.Three (g, h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)
          | (Digit.Four (a, b, c, d), e, f, Digit.Four (g, h, i, j)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), Elem.node2 (i, j), t2)

      and addDigits3 (t1, d1, x, y, z, d2, t2) =
         case (d1, x, y, z, d2) of
            (Digit.One a, b, c, d, Digit.One e) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.One a, b, c, d, Digit.Two (e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.One a, b, c, d, Digit.Three (e, f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.One a, b, c, d, Digit.Four (e, f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)

          | (Digit.Two (a, b), c, d, e, Digit.One f) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Two (a, b), c, d, e, Digit.Two (f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Two (a, b), c, d, e, Digit.Three (f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Two (a, b), c, d, e, Digit.Four (f, g, h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)

          | (Digit.Three (a, b, c), d, e, f, Digit.One g) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Three (a, b, c), d, e, f, Digit.Two (g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Three (a, b, c), d, e, f, Digit.Three (g, h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)
          | (Digit.Three (a, b, c), d, e, f, Digit.Four (g, h, i, j)) =>  appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), Elem.node2 (i, j), t2)

          | (Digit.Four (a, b, c, d), e, f, g, Digit.One h) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Four (a, b, c, d), e, f, g, Digit.Two (h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)
          | (Digit.Four (a, b, c, d), e, f, g, Digit.Three (h, i, j)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), Elem.node2 (i, j), t2)
          | (Digit.Four (a, b, c, d), e, f, g, Digit.Four (h, i, j, k)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), Elem.node2 (j, k), t2)

      and addDigits4 (t1, d1, x, y, z, w, d2, t2) =
         case (d1, x, y, z, w, d2) of
            (Digit.One a, b, c, d, e, Digit.One f) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.One a, b, c, d, e, Digit.Two (f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.One a, b, c, d, e, Digit.Three (f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.One a, b, c, d, e, Digit.Four (f, g, h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)

          | (Digit.Two (a, b), c, d, e, f, Digit.One g) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Two (a, b), c, d, e, f, Digit.Two (g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Two (a, b), c, d, e, f, Digit.Three (g, h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)
          | (Digit.Two (a, b), c, d, e, f, Digit.Four (g, h, i, j)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), Elem.node2 (i, j), t2)

          | (Digit.Three (a, b, c), d, e, f, g, Digit.One h) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)
          | (Digit.Three (a, b, c), d, e, f, g, Digit.Two (h, i)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)
          | (Digit.Three (a, b, c), d, e, f, g, Digit.Three (h, i, j)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), Elem.node2 (i, j), t2)
          | (Digit.Three (a, b, c), d, e, f, g, Digit.Four (h, i, j, k)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), Elem.node2 (j, k), t2)

          | (Digit.Four (a, b, c, d), e, f, g, h, Digit.One i) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), t2)
          | (Digit.Four (a, b, c, d), e, f, g, h, Digit.Two (i, j)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), Elem.node2 (i, j), t2)
          | (Digit.Four (a, b, c, d), e, f, g, h, Digit.Three (i, j, k)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), Elem.node2 (j, k), t2)
          | (Digit.Four (a, b, c, d), e, f, g, h, Digit.Four (i, j, k, l)) => appTree4 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node3 (g, h, i), Elem.node3 (j, k, l), t2)

      and appTree1 (Empty, a, ys) = consTree (a, ys)
        | appTree1 (xs, a, Empty) = snocTree (xs, a)
        | appTree1 (Single x, a, ys) = consTree (x, consTree (a, ys))
        | appTree1 (xs, a, Single y) = snocTree (snocTree (xs, a), y)
        | appTree1 (Deep d1, a, Deep d2) =
         deep (#left d1,
               Lazy.delay (fn () =>
                  addDigits1 (Lazy.force (#subtree d1),
                              #right d1,
                              a,
                              #left d2,
                              Lazy.force (#subtree d2))),
               #right d2)

      and appTree2 (Empty, a, b, ys) = consTree (a, consTree (b, ys))
        | appTree2 (xs, a, b, Empty) = snocTree (snocTree (xs, a), b)
        | appTree2 (Single x, a, b, ys) = consTree (x, consTree (a, consTree (b, ys)))
        | appTree2 (xs, a, b, Single y) = snocTree (snocTree (snocTree (xs, a), b), y)
        | appTree2 (Deep d1, a, b, Deep d2) =
         deep (#left d1,
               Lazy.delay (fn () =>
                  addDigits2 (Lazy.force (#subtree d1),
                              #right d1,
                              a, b,
                              #left d2,
                              Lazy.force (#subtree d2))),
               #right d2)

      and appTree3 (Empty, a, b, c, ys) = consTree (a, consTree (b, consTree (c, ys)))
        | appTree3 (xs, a, b, c, Empty) = snocTree (snocTree (snocTree (xs, a), b), c)
        | appTree3 (Single x, a, b, c, ys) = consTree (x, consTree (a, consTree (b, consTree (c, ys))))
        | appTree3 (xs, a, b, c, Single y) = snocTree (snocTree (snocTree (snocTree (xs, a), b), c), y)
        | appTree3 (Deep d1, a, b, c, Deep d2) =
         deep (#left d1,
               Lazy.delay (fn () =>
                  addDigits3 (Lazy.force (#subtree d1),
                              #right d1,
                              a, b, c,
                              #left d2,
                              Lazy.force (#subtree d2))),
               #right d2)

      and appTree4 (Empty, a, b, c, d, ys) = consTree (a, consTree (b, consTree (c, consTree (d, ys))))
        | appTree4 (xs, a, b, c, d, Empty) = snocTree (snocTree (snocTree (snocTree (xs, a), b), c), d)
        | appTree4 (Single x, a, b, c, d, ys) = consTree (x, consTree (a, consTree (b, consTree (c, consTree (d, ys)))))
        | appTree4 (xs, a, b, c, d, Single y) = snocTree (snocTree (snocTree (snocTree (snocTree (xs, a), b), c), d), y)
        | appTree4 (Deep d1, a, b, c, d, Deep d2) =
         deep (#left d1,
               Lazy.delay (fn () =>
                  addDigits4 (Lazy.force (#subtree d1),
                              #right d1,
                              a, b, c, d,
                              #left d2,
                              Lazy.force (#subtree d2))),
               #right d2)

      fun addDigits0 (t1, d1, d2, t2) =
         case (d1, d2) of
            (Digit.One a, Digit.One b) => appTree1 (t1, Elem.node2 (a, b), t2)
          | (Digit.One a, Digit.Two (b, c)) => appTree1 (t1, Elem.node3 (a, b, c), t2)
          | (Digit.One a, Digit.Three (b, c, d)) => appTree2 (t1, Elem.node2 (a, b), Elem.node2 (c, d), t2)
          | (Digit.One a, Digit.Four (b, c, d, e)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)

          | (Digit.Two (a, b), Digit.One c) => appTree1 (t1, Elem.node3 (a, b, c), t2)
          | (Digit.Two (a, b), Digit.Two (c, d)) => appTree2 (t1, Elem.node2 (a, b), Elem.node2 (c, d), t2)
          | (Digit.Two (a, b), Digit.Three (c, d, e)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.Two (a, b), Digit.Four (c, d, e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)

          | (Digit.Three (a, b, c), Digit.One d) => appTree2 (t1, Elem.node2 (a, b), Elem.node2 (c, d), t2)
          | (Digit.Three (a, b, c), Digit.Two (d, e)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.Three (a, b, c), Digit.Three (d, e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Three (a, b, c), Digit.Four (d, e, f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)

          | (Digit.Four (a, b, c, d), Digit.One e) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), t2)
          | (Digit.Four (a, b, c, d), Digit.Two (e, f)) => appTree2 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), t2)
          | (Digit.Four (a, b, c, d), Digit.Three (e, f, g)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node2 (d, e), Elem.node2 (f, g), t2)
          | (Digit.Four (a, b, c, d), Digit.Four (e, f, g, h)) => appTree3 (t1, Elem.node3 (a, b, c), Elem.node3 (d, e, f), Elem.node2 (g, h), t2)

      fun append (Empty, ys) = ys
        | append (xs, Empty) = xs
        | append (Single x, ys) = consTree (x, ys)
        | append (xs, Single y) = snocTree (xs, y)
        | append (Deep d1, Deep d2) =
         deep (#left d1,
               Lazy.delay (fn () =>
                  addDigits0 (Lazy.force (#subtree d1),
                              #right d1,
                              #left d2,
                              Lazy.force (#subtree d2))),
               #right d2)

      fun deepL (NONE, m, sf) = rotL (Lazy.force m, sf)
        | deepL (SOME pr, m, sf) = deep (pr, m, sf)

      fun searchDigit p =
         Digit.search
         (fn (a, b) => a <+> Elem.measure b)
         (fn (a, b) => Elem.measure a <+> b)
         p

      fun searchTree p (vl, t, vr) =
         case t of
            Empty => raise Fail "FingerTree.searchTree: Empty"
          | Single x => (Empty, x, Empty)
          | Deep {left, right, subtree, ...} =>
               let
                  val vm = measure (Lazy.force subtree)
                  val vsr = measureDigit right <+> vr
                  val vlp = vl <+> measureDigit left
                  val vlpm = vlp <+> vm
                  val vmsr = vm <+> vsr
               in
                  if p (vlp, vmsr)
                     then
                        case searchDigit p (vl, left, vmsr) of
                           (SOME l, x, r) => (fromDigit l, x, deepL (r, subtree, right))
                         | (NONE, x, r) => (Empty, x, deepL (r, subtree, right))
                  else raise Fail "NYI"
               end

      (* Split *)
      (* XXX *)

   end

structure Seq = FingerTree(
   struct
      structure Measure =
         struct
            type t = int

            val zero = 0
            val op + = Int.+
         end

      structure Item =
         struct
            type 'a t = 'a

            fun measure _ = 1
         end
   end)

(* vim: set tw=0 ts=3 sw=3: *)
