
structure Gen: GEN =
struct
   type 'a t = Random.rand -> 'a Tree.t

   fun map f g s = Tree.map f (g s)

   fun pure x _ = Tree.pure x

   fun ap (f, x) s = Tree.ap (f s, x s)

   fun curry f x y = f (x, y)
   fun curry3 f x y z = f (x, y, z)

   fun map2 f (x, y) s = Tree.map2 f (x s, y s)

   fun map3 f (x, y, z) = ap (ap (map (curry3 f) x, y), z)

   fun bind x f s = Tree.bind (x s) (fn a => f a s)

   fun join g = bind g (fn x => x)

   fun sequence xs s = Tree.sequence (List.map (fn g => g s) xs)

   fun delay f s = f () s

   fun fix f =
      let
         fun go x s = f go x s
      in
         go
      end

   fun replicate (n, g) s =
      let
         fun go (n, acc) =
            if n <= 0
               then acc
            else go (n - 1, Tree.map2 op :: (g s, acc))
      in
         go (n, Tree.pure [])
      end

   fun pair (g1, g2) s = Tree.map2 (fn p => p) (g1 s, g2 s)

   fun generate (s, n, g) = Tree.root (replicate (n, g) s) 

   fun generate1 (s, g) = Tree.root (g s)

   fun generateTree (s, g) = g s

   fun make {gen, shrink} s = Tree.makePrimitive shrink (gen s)

   structure ShrinkInt = ShrinkNumberFn(struct open Int type t = int end)

   val smallNat =
      let
         fun gen s =
            if Random.randReal s < 0.75
               then Random.randRange (0, 10) s
            else Random.randRange (0, 100) s
      in
         make {gen = gen, shrink = ShrinkInt.shrink {dest = 0}}
      end

   val nat =
      let
         fun gen s =
            let
               val p = Random.randReal s
            in
               if p < 0.5
                  then Random.randRange (0, 10) s
               else if p < 0.75
                  then Random.randRange (0, 100) s
               else if p < 0.95
                  then Random.randRange (0, 1000) s
               else Random.randRange (0, 10000) s
            end
      in
         make {gen = gen, shrink = ShrinkInt.shrink {dest = 0}}
      end

   fun bigNat s =
      if Random.randReal s < 0.75
         then nat s
      else make {gen = Random.randRange (0, 1000000),
                 shrink = ShrinkInt.shrink {dest = 0}} s

   val int =
      make {gen = Random.randInt, shrink = ShrinkInt.shrink {dest = 0}}

   fun intRange {origin, min, max} =
      make {gen = Random.randRange (min, max), shrink = ShrinkInt.shrink {dest = origin}}

   fun unit _ = Tree.pure ()

   fun bool s =
      if Random.randRange (0, 1) s = 0
         then Tree.make (true, Seq.singleton (Tree.pure false))
      else Tree.pure false

   fun oneof [] = raise Fail "Gen.oneof: Empty list"
     | oneof gs =
      bind (intRange {origin = 0, min = 0, max = List.length gs - 1})
      (fn n => List.nth (gs, n))

   fun element [] = raise Fail "Gen.element: Empty list"
     | element xs =
      map (fn n => List.nth (xs, n))
      (intRange {origin = 0, min = 0, max = List.length xs - 1})

   fun frequency [] = raise Fail "Gen.frequency: Empty list"
     | frequency xs =
      let
         val sum = List.foldr (fn ((n, _), m) => n + m) 0 xs
         val () =
            if sum < 1
               then raise Fail "Gen.frequency: total weight < 1"
            else ()
         fun f i =
            let
               fun go (acc, (x, g) :: xs) =
                  if i < acc + x
                     then g
                  else go (acc + x, xs)
                 | go (_, []) = raise Fail "Gen.frequency: internal error"
            in
               go (0, xs)
            end
      in
         bind (intRange {origin = 0, min = 0, max = sum}) f
      end
end
(* vim: set tw=0 ts=3 sw=3: *)
