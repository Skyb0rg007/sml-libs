
structure MSS =
struct

infix ++

fun lift2 f (xs, ys) =
   List.concat (List.map (fn y => List.map (fn x => f (x, y)) xs) ys)

val _: ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list = lift2

fun merge _ ([], ys) = ys
  | merge _ (xs, []) = xs
  | merge cmp (x::xs, y::ys) =
   case cmp (x, y) of
      LESS => x :: merge cmp (xs, y::ys)
    | EQUAL => x :: merge cmp (xs, y::ys)
    | GREATER => y :: merge cmp (x::xs, ys)

val _: ('a * 'a -> order) -> 'a list * 'a list -> 'a list = merge

local
   fun f1 x = [[x]]
   fun p1 ((s1, i1, t1, d1), (s2, i2, t2, d2)) = s1 @ s2 @ lift2 op @ (t1, i2)

   fun f2 x = [[x]]
   fun p2 ((s1, i1, t1, d1), (s2, i2, t2, d2)) = i1 @ List.map (fn i => d1 @ i) i2

   fun f3 x = [[x]]
   fun p3 ((s1, i1, t1, d1), (s2, i2, t2, d2)) = List.map (fn t => d1 @ t) t1 @ t2

   fun f4 x = [x]
   fun p4 ((s1, i1, t1, d1), (s2, i2, t2, d2)) = d1 @ d2

   fun f x = (f1 x, f2 x, f3 x, f4 x)
   fun p (a, b) = (p1 (a, b), p2 (a, b), p3 (a, b), p4 (a, b))

   fun f1' (i, x) = [([i], x)]
   fun p1' ((s1, i1, t1, d1), (s2, i2, t2, d2)) = s1 @ s2 @ lift2 op @ (t1, i2)
   fun f2' (i, x) = [([i], x)]
   fun p2' ((s1, i1, t1, d1), (s2, i2, t2, d2)) = i1 @ List.map (fn i => d1 + i) i2
   fun f3' (i, x) = [([i], x)]
   fun f4' (i, x) = ([i], x)
in
   fun segs xs = #1 (List.foldl (fn (x, acc) => p (f x, acc)) ([], [], [], []) xs)
end

end

(* vim: set tw=0 ts=3 sw=3: *)
