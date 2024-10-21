
functor GraphFn(Key: ORD_KEY) =
struct

structure Set = RedBlackSetFn(Key)
structure Map = RedBlackMapFn(Key)

datatype 'a t = T of {
      out: 'a Map.map Map.map,
      in_: 'a Map.map Map.map,
      levels: level Map.map
   }

withtype level = int

val empty = T {out = Map.empty, in_ = Map.empty, levels = Map.empty}

fun successors (T {out, ...}, x) =
   case Map.find (out, x) of
      NONE => []
    | SOME m => Map.listItemsi m

fun predecessors (T {in_, ...}, x) =
   case Map.find (in_, x) of
      NONE => []
    | SOME m => Map.listItemsi m

fun level (T {levels, ...}, x) =
   Option.getOpt (Map.find (levels, x), 0)

fun connected (T {in_, out, ...}) =
   Map.listKeys (Map.unionWith #1 (in_, out))

fun size (T {in_, out, ...}) =
   Map.numItems (Map.unionWith #1 (in_, out))

fun numEdges (T {in_, out, ...}) =
   let
      val sizeIn = 
         Map.foldl (fn (m, acc) => Map.numItems m + acc) 0 in_
      val sizeOut =
         Map.foldl (fn (m, acc) => Map.numItems m + acc) sizeIn out
   in
      Int.quot (sizeOut, 2)
   end

fun insertEdge (x, y) edge (g as T {in_, out, levels}) =
   let
      val levels = Map.insertWith #2 (levels, x, ~1)
      val levels = Map.insertWith #2 (levels, y, 0)
      fun getLevel z = Option.getOpt (Map.find (levels, z), 0)
      val diff = getLevel y - 1 - getLevel x
      fun adjustLevel (x, m) = Map.insert (m, x, Map.lookup (m, x) + diff)
      val preds = [] (* reversePostOrder [x] (map #1 o getIncoming) *)
      fun adjustLevels m =
         List.foldl adjustLevel m preds
   in
      T {
         in_ =
            Map.insertWith (Map.unionWith #1)
               (Map.insertWith #2 (in_, x, Map.empty),
                y,
                Map.singleton (x, edge)),
         out =
            Map.insertWith (Map.unionWith #1)
               (Map.insertWith #2 (out, y, Map.empty),
                x,
                Map.singleton (y, edge)),
         levels =
            if diff >= 0
               then levels
               else adjustLevels levels
      }
   end

val _ : 'a t * Key.ord_key -> (Key.ord_key * 'a) list = successors
val _ : 'a t * Key.ord_key -> (Key.ord_key * 'a) list = predecessors
val _ : 'a t * Key.ord_key -> level = level
val _ : 'a t -> Key.ord_key list = connected
val _ : 'a t -> int = size
val _ : 'a t -> int = numEdges

end

structure Graph = GraphFn(
   struct
      type ord_key = IntInf.int
      val compare = IntInf.compare
   end)

(* vim: set tw=0 ts=3 sw=3: *)
