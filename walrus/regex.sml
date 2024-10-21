
signature REGEX =
   sig
      type t

      val empty: t
      val epsilon: t
      val byteSet: ByteSet.t -> t
      val alt: t * t -> t
      val seq: t * t -> t
      val & : t * t -> t
      val complement: t -> t
      val many: t -> t
   end

structure Regex =
   struct
      structure ByteSetSet = RedBlackSetFn(
         struct
            type ord_key = ByteSet.t

            val compare = ByteSet.compare
         end)

      structure ByteSetSet =
         struct
            open ByteSetSet

            fun cross (s1, s2) =
               foldl (fn (x, acc) =>
                  foldl (fn (y, acc) =>
                     add (acc, ByteSet.intersect (x, y)))
                     acc s2)
                  empty s1

            val trivial = singleton ByteSet.universe
         end

      datatype t =
         Set of ByteSet.t
       | Epsilon
       | Closure of t
       | Concat of t list
       | Or of t list

      fun compare (Set s1, Set s2) = ByteSet.compare (s1, s2)
        | compare (Set _, _) = LESS
        | compare (_, Set _) = GREATER
        | compare (Epsilon, Epsilon) = EQUAL
        | compare (Epsilon, _) = LESS
        | compare (_, Epsilon) = GREATER
        | compare (Closure a, Closure b) = compare (a, b)
        | compare (Closure _, _) = LESS
        | compare (_, Closure _) = GREATER
        | compare (Concat a, Concat b) = List.collate compare (a, b)
        | compare (Concat _, _) = LESS
        | compare (_, Concat _) = GREATER
        | compare (Or a, Or b) = List.collate compare (a, b)

      structure Map = RedBlackMapFn(
         struct
            type ord_key = t

            val compare = compare
         end)

      val empty = Set ByteSet.empty

      val epsilon = Epsilon

      fun many Epsilon = Epsilon
        | many (r as Closure _) = r
        | many (r as Set s) = if ByteSet.isEmpty s then Epsilon else Closure r
        | many r = Closure r

      local
         fun seq' (Epsilon, r) = r
           | seq' (r, Epsilon) = r
           | seq' (Concat a, Concat b) = Concat (a @ b)
           | seq' (a, Concat b) = Concat (a :: b)
           | seq' (Concat a, b) = Concat (a @ [b])
           | seq' (a, b) = Concat [a, b]
      in
         fun seq (a as Set s, b) =
            if ByteSet.isEmpty s
               then empty
            else seq' (a, b)
           | seq (a, b as Set s) =
            if ByteSet.isEmpty s
               then empty
            else seq' (a, b)
           | seq (a, b) = seq' (a, b)
      end

      local
         fun merge ([], ys) = ys
           | merge (xs, []) = xs
           | merge (Set x :: xs, Set y :: ys) =
            Set (ByteSet.union (x, y)) :: merge (xs, ys)
           | merge (x :: xs, y :: ys) =
            if compare (x, y) = LESS
               then x :: merge (xs, ys)
            else y :: merge (x :: xs, ys)

         fun alt' (Set a, Set b) = Set (ByteSet.union (a, b))
           | alt' (Or a, Or b) = Or (merge (a, b))
           | alt' (Or a, b) = Or (merge (a, [b]))
           | alt' (a, Or b) = Or (merge ([a], b))
           | alt' (a, b) = Or (merge ([a], [b]))
      in
         fun alt (a as Set s, b) =
            if ByteSet.isEmpty s
               then empty
            else alt' (a, b)
           | alt (a, b as Set s) =
            if ByteSet.isEmpty s
               then empty
            else alt' (a, b)
           | alt (a, b) = alt' (a, b)
      end

      fun nullable (Set _) = false
        | nullable Epsilon = true
        | nullable (Closure _) = true
        | nullable (Concat rs) = List.all nullable rs
        | nullable (Or rs) = List.exists nullable rs

      fun delta r =
         if nullable r
            then Epsilon
         else empty

      fun derivative a =
         let
            fun da (Set s) =
               if ByteSet.member (s, a)
                  then Epsilon
               else empty
              | da Epsilon = empty
              | da (Closure r) = seq (da r, Closure r)
              | da (Concat [r]) = da r
              | da (Concat (r :: rs)) =
               alt (seq (da r, Concat rs),
                    seq (delta r, da (Concat rs)))
              | da (Or [r]) = da r
              | da (Or (r :: rs)) =
               alt (da r, da (Or rs))
              | da (Or []) = raise Fail "empty Or"
              | da (Concat []) = raise Fail "empty Concat"
         in
            da
         end

      fun derivatives (rs: t list): (t list * ByteSet.t) list =
         let
            fun ds (Set s) = ByteSetSet.fromList [s, ByteSet.complement s]
              | ds Epsilon = ByteSetSet.trivial
              | ds (Closure r) = ds r
              | ds (Concat [r]) = ds r
              | ds (Concat (r :: rs)) =
               if nullable r
                  then ByteSetSet.cross (ds r, ds (Concat rs))
               else ds r
              | ds (Or rs) = crossDs rs
              | ds (Concat []) = raise Fail "empty Concat"

            and crossDs rs = List.foldl
                  (fn (r, acc) => ByteSetSet.cross (ds r, acc))
                  ByteSetSet.trivial
                  rs

            fun classes ([], acc) = acc
              | classes (set :: sets, acc) =
               case ByteSet.first set of
                  NONE => classes (sets, acc)
                | SOME rep =>
                     let
                        val derivs = List.map (derivative rep) rs
                     in
                        classes (sets, (derivs, set) :: acc)
                     end
         in
            classes (ByteSetSet.listItems (crossDs rs), [])
         end

   end

(* vim: set tw=0 ts=3 sw=3: *)
