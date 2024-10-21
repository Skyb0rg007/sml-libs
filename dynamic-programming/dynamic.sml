
structure Dynamic =
struct

local
   structure IntAdditive: ORDERED_MONOID where type t = int =
      struct
         type t = int

         val one = 0
         val op * = Int.+
         val compare = Int.compare
      end

   structure Mat = Matrix(val n = 5)

   structure Dist = TropicalSemiring(IntAdditive)
   structure MatDist = MatrixSemiring(structure M = Mat structure R = Dist)

   structure P = TupleMonoid(
      struct
         structure A = IntAdditive
         structure B = ListMonoid(type t = int * int)
      end)
   structure Path = TropicalSemiring(
      struct
         open P

         fun compare ((n, _), (m, _)) = Int.compare (n, m)
      end)
   structure MatPath = MatrixSemiring(structure M = Mat structure R = Path)

   structure Path' = TropicalSemiring(
      struct
         open P

         fun compare ((n, _), (m, _)) = Int.compare (m, n)
      end)
   structure MatPath' = MatrixSemiring(structure M = Mat structure R = Path)
in
   fun shortestDistances edges = MatDist.closure (Mat.adjacency Dist.one edges)

   fun shortestPath edges =
      let
         fun withEdge (e, a) = (e, SOME (a, [e]))
      in
         MatPath.closure (Mat.adjacency Path.one (List.map withEdge edges))
      end

   fun largestPath edges =
      let
         fun withEdge (e, a) = (e, SOME (a, [e]))
      in
         MatPath'.closure (Mat.adjacency Path'.one (List.map withEdge edges))
      end
end

end

(* vim: set tw=0 ts=3 sw=3: *)
