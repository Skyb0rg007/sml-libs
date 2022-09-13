(* TODO: Flush out this api some more *)

signature TABLE =
sig

type key
type 'a table

val new: int -> 'a table
val find: 'a table * key -> 'a option
val insert: 'a table * key * 'a -> unit
val foldi: (key * 'a * 'b -> 'b) -> 'b -> 'a table -> 'b
val toList: 'a table -> (key * 'a) list

end

(* vim: set tw=0 ts=3 sw=3: *)
