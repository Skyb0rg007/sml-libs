
functor GraphFn(Key: ORD_KEY) :>
sig
   type t

   val empty: t
   val insertEdge: t * Key.ord_key * Key.ord_key -> t
   val getChildren: t * Key.ord_key -> Key.ord_key list
   val getParents: t * Key.ord_key -> Key.ord_key list
   val listParents: t -> Key.ord_key list
   val reversePostOrder: Key.ord_key -> (Key.ord_key -> Key.ord_key list) -> Key.ord_key list
end =
struct
   structure M = RedBlackMapFn(Key)
   structure S = RedBlackSetFn(Key)

   datatype t = T of {
      children: Key.ord_key list M.map,
      parents: Key.ord_key list M.map,
      nodes: S.set
   }

   val empty = T {children = M.empty, parents = M.empty, nodes = S.empty}

   fun insertEdge (T {children, parents, nodes}, x, y) =
      T {
         children = M.insertWith op @ (children, x, [y]),
         parents = M.insertWith op @ (parents, y, [x]),
         nodes = S.add (S.add (nodes, x), y)
      }

   fun getChildren (T {children, ...}, x) =
      Option.getOpt (M.find (children, x), [])

   fun getParents (T {parents, ...}, x) =
      Option.getOpt (M.find (parents, x), [])

   fun reversePostOrder' xs children =
      let
         fun go [] rpo visited = (rpo, visited)
           | go (x::xs) rpo visited =
            if S.member (visited, x)
               then go xs rpo visited
            else
               let
                  val xs' = children x
                  val (rpo', visited') = go xs' rpo (S.add (visited, x))
               in
                  go xs (x :: rpo') visited'
               end
      in
         #1 (go xs [] S.empty)
      end

   fun reversePostOrder x = reversePostOrder' [x]

   fun listParents (gr as T {children, parents, ...}) =
      let
         fun hasParents x = M.inDomain (parents, x)
         val ancestors =
            List.filter (fn x => not (hasParents x)) (M.listKeys children)
      in
         reversePostOrder' ancestors (fn x => getChildren (gr, x))
      end
end

functor OrderedBagFn(Key: ORD_KEY) :>
sig
   type t

   val empty: t
   val insert: t * Key.ord_key -> t
   val inserts: t * Key.ord_key list -> t
   val inOrder: t * (Key.ord_key * 'a) list -> (Key.ord_key * 'a) list
end =
struct
   structure M = RedBlackMapFn(Key)

   datatype t = T of int M.map * int

   val empty = T (M.empty, 0)

   fun insert (T (xs, n), x) =
      T (M.insertWith (fn (_, old) => old) (xs, x, n), n + 1)

   fun inserts (ob, xs) = List.foldl (Fn.flip insert) ob xs

   fun inOrder (T (bag, _), xs) =
      let
         fun gt ((a, _), (b, _)) = a > b
         fun f (x, y) =
            case M.find (bag, x) of
               NONE => NONE
             | SOME r => SOME (r, (x, y))
      in
         List.map #2 (ListMergeSort.sort gt (List.mapPartial f xs))
      end
end

structure Vault :>
sig
   type locker
   type 'a key
   type vault

   val newKey: unit -> 'a key
   val lookup: vault * 'a key -> 'a option
   val insert: vault * 'a key * 'a -> vault
   val adjust: ('a -> 'a) -> vault * 'a key -> vault
   val delete: vault * 'a key -> vault
end =
struct
   datatype locker = Locker of int * (unit -> unit)
   datatype 'a key = Key of int * 'a option ref

   structure M = IntRedBlackMap
   datatype vault = Vault of locker M.map

   val counter = ref 0
   fun fresh () = !counter before counter := !counter + 1

   fun lock (Key (u, r), x) = Locker (u, fn () => r := SOME x)

   fun unlock (Key (k, r), Locker (k', m)) =
      if k = k'
         then (m (); !r)
      else NONE

   fun newKey () =
      Key (fresh (), ref NONE)

   fun lookup (Vault m, key as Key (k, _)) =
      case M.find (m, k) of
         NONE => NONE
       | SOME l => unlock (key, l)

   fun insert (Vault m, key as Key (k, _), x) =
      Vault (M.insert (m, k, lock (key, x)))

   fun adjust f (Vault m, key as Key (k, _)) =
      case M.find (m, k) of
         NONE => Vault m
       | SOME l =>
            case unlock (key, l) of
               NONE => Vault (#1 (M.remove (m, k)))
             | SOME x => Vault (M.insert (m, k, lock (key, f x)))

   fun delete (Vault m, Key (k, _)) =
      if M.inDomain (m, k)
         then Vault (#1 (M.remove (m, k)))
      else Vault m
end

structure Low =
struct
   structure Time =
   struct
      datatype t = T of IntInf.int

      val agesAgo = T ~1
      val beginning = T 0
      fun next (T n) = T (n + 1)
      fun (T x) + (T y) = T (IntInf.+ (x, y))
   end
end

(* vim: set ft=sml sw=3 ts=3 tw=0: *)
