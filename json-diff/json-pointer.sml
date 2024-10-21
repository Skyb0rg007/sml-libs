
structure JSONPointer =
struct

structure J = JSON

datatype key =
   OKEY of string
 | AKEY of int

type path = key list

fun keyToString (AKEY i) = Int.toString i
  | keyToString (OKEY t) =
   let
      fun esc #"~" = "~0"
        | esc #"/" = "~1"
        | esc c = String.str c
   in
      String.translate esc t
   end

fun toString [] = ""
  | toString path = "/" ^ String.concatWithMap "/" keyToString path

exception PointerFailure of path * JSON.value

fun get [] v = v
  | get (p as AKEY i :: path) (j as JSON.ARRAY arr) =
   (get path (List.nth (arr, i))
    handle Subscript => raise PointerFailure (p, j))
  | get (p as OKEY k :: path) (j as JSON.OBJECT obj) =
   (case List.find (fn (k', _) => k = k') obj of
      NONE => raise PointerFailure (p, j)
    | SOME (_, v) => get path v)
  | get p j = raise PointerFailure (p, j)

end

(* vim: set tw=0 ts=3 sw=3: *)
