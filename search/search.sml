
structure Search =
struct

datatype ('c, 'a) container = Container of {
      empty : 'c,
      push : 'c * 'a -> 'c,
      pop : 'c -> ('a * 'c) option
   }

val listContainer : ('a list, 'a) container = Container {
      empty = [],
      push = fn (xs, x) => x :: xs,
      pop = fn [] => NONE | x :: xs => SOME (x, xs)
   }

fun generalizedSearch {empty, key, better, next, found, initial} =
   let
      val initialState = (initial, empty, [key initial])
      (* val endOpt = findIterate (nextSearchState better key next) *)
   in
      ()
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
