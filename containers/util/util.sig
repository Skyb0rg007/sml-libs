
signature UTIL =
sig

(* [Heuristic] returns true if the arguments point to the same value
 * If `ptrEq` returns false, nothing is known about the arguments *)
val ptrEq: 'a * 'a -> bool

end

(* vim: set tw=0 ts=3 sw=3: *)
