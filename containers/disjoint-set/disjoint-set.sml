
structure DisjointSet: DISJOINT_SET =
struct
   datatype 'a parent =
      Parent of 'a t
    | Root of int * 'a

   withtype 'a t = 'a parent ref

   fun make v = ref (Root (0, v))

   fun isRepresentative (ref (Root _)) = true
     | isRepresentative _ = false

   fun representative s =
      case !s of
         Root _ => s
       | Parent p =>
            let
               val r = representative p
            in
               s := Parent r
               ; r
            end

   fun same (x: 'a t, y) = x = y orelse representative x = representative y

   fun shallowEq (x: 'a t, y) = x = y

   fun get s =
      case !(representative s) of
         Root (_, x) => x
       | _ => raise Fail "Bug in DisjointSet.representative"

   fun set (s, v) =
      case representative s of
         r as ref (Root (rank, _)) => r := Root (rank, v)
       | _ => raise Fail "Bug in DisjointSet.representative"

   fun union (s1, s2) =
      let
         val r1 = representative s1
         val r2 = representative s2
      in
         if r1 = r2
            then ()
         else
            case (!r1, !r2) of
               (_, Parent _) => raise Fail "Bug in DisjointSet.representative"
             | (Parent _, _) => raise Fail "Bug in DisjointSet.representative"
             | (Root (rank1, x), Root (rank2, _)) =>
                  case Int.compare (rank1, rank2) of
                     LESS => r1 := Parent r2
                   | GREATER => r2 := Parent r1
                   | EQUAL => (r1 := Root (rank1 + 1, x); r2 := Parent r1)
      end

   val ! = get
   val op := = set
end

(* vim: set tw=0 ts=3 sw=3: *)
