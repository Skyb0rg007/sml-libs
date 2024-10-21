
structure Cell: CELL =
struct

datatype 'a change =
   Changed of 'a
 | Unchanged

datatype 'a t = T of {
      merge: 'a * 'a -> 'a change,
      value: 'a ref,
      prop: ('a -> unit) ref
   }

fun content (T {value, ...}) = !value

fun new value merge = T {
      merge = merge,
      value = ref value,
      prop = ref (fn _ => ())
   }

fun write (T {merge, value, prop}, y) =
   let
      val x = !value
   in
      case merge (x, y) of
         Unchanged => ()
       | Changed z => (value := z; !prop z)
   end

fun watch (T {merge, value, prop}) k =
   let
      val propagate = !prop
   in
      prop := (fn x => (propagate x; k x))
      ; k (!value)
   end

fun watch2 cell1 cell2 k =
   (watch cell1 (fn x => k (x, content cell2))
    ; watch cell2 (fn y => k (content cell1, y)))

fun watch3 cell1 cell2 cell3 k =
   (watch cell1 (fn x => k (x, content cell2, content cell3))
    ; watch cell2 (fn y => k (content cell1, y, content cell3))
    ; watch cell3 (fn z => k (content cell1, content cell2, z)))

fun equals (T {value = r1, ...}, T {value = r2, ...}) = r1 = r2

end

(* vim: set tw=0 ts=3 sw=3: *)
