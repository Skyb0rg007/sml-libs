
structure Datalog =
struct

datatype term =
   Var of int
 | Apply of Atom.atom * term vector


end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
