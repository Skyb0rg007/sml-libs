
structure Walrus =
struct
   structure IntMap = IntRedBlackMap
   structure IntSet = IntRedBlackSet

   structure Grammar =
      struct
         datatype name =
            Epsilon
          | Error
          | Dummy
          | Start
          | Nonterminal of int
          | Terminal of int

         structure K =
            struct
               type ord_key = name

               fun compare (Epsilon, Epsilon) = EQUAL
                 | compare (Epsilon, _) = LESS
                 | compare (_, Epsilon) = GREATER
                 | compare (Error, Error) = EQUAL
                 | compare (Error, _) = LESS
                 | compare (_, Error) = GREATER
                 | compare (Dummy, Dummy) = EQUAL
                 | compare (Dummy, _) = LESS
                 | compare (_, Dummy) = GREATER
                 | compare (Start, Start) = EQUAL
                 | compare (Start, _) = LESS
                 | compare (_, Start) = GREATER
                 | compare (Nonterminal n, Nonterminal m) = Int.compare (n, m)
                 | compare (Nonterminal _, _) = LESS
                 | compare (_, Nonterminal _) = GREATER
                 | compare (Terminal n, Terminal m) = Int.compare (n, m)
            end

         structure NameSet = RedBlackSetFn(K)
         structure NameMap = RedBlackMapFn(K)

         datatype production = Production of {
               lhs: name,
               rhs: name list
            }

         datatype t = Grammar of {
               productions: production list IntMap.map,
               terminals: int list,
               nonterminals: int list
            }

         fun joinSymSets f xs =
            List.foldl
               (fn (h, b) =>
                  if NameSet.member (h, Epsilon)
                     then NameSet.union (NameSet.delete (h, Epsilon), b)
                  else h)
               (NameSet.singleton Epsilon)
               (List.map f xs)

         fun mkClosure eq f x =
            let
               val y = f x
            in
               if eq (x, y)
                  then x
               else mkClosure eq f y
            end

         fun getNext (prods: production list IntMap.map) (env: NameSet.set NameMap.map): NameSet.set NameMap.map =
            let
               fun f Error = NameSet.singleton Error
                 | f (t as Terminal _) = NameSet.singleton t
                 | f t = NameMap.lookup (env, t)
               fun next (t as Terminal _) = NameSet.singleton t
                 | next (Nonterminal n) =
                  List.foldr
                     (fn (Production {rhs, ...}, acc) =>
                        NameSet.union (joinSymSets f rhs, acc))
                     NameSet.empty
                     (IntMap.lookup (prods, n))
                 | next _ = raise Fail "impossible"
            in
               NameMap.mapi (next o #1) env
            end

         fun mkFirst (Grammar {productions, nonterminals, ...}) =
            let
               fun eq (m1, m2) =
                  NameMap.collate NameSet.compare (m1, m2) = EQUAL
               val m0 =
                  List.foldl
                     (fn (n, acc) => NameMap.insert (acc, Nonterminal n, NameSet.empty))
                     NameMap.empty
                     nonterminals
               val env = mkClosure eq (getNext productions) m0
            in
               joinSymSets (fn h =>
                  case NameMap.find (env, h) of
                     NONE => NameSet.singleton h
                   | SOME x => x)
            end
      end

   structure Tabular =
      struct
         datatype item = Item of {rule: int, dot: int}

         datatype action =
            Shift of {state: int}
          | Reduce of {rule: int}
          | Accept
          | Fail
      end


   structure G = Grammar
   val g = G.Grammar {
      productions = List.foldl IntMap.insert' IntMap.empty [
         (0, [G.Production {lhs = G.Nonterminal 0, rhs = [G.Terminal 1]}])
      ],
      nonterminals = [0],
      terminals = [0, 1]
   }
end

(* vim: set tw=0 ts=3 sw=3: *)
