
structure DynArray: DYN_ARRAY =
struct

datatype 'a t = T of {arr: 'a array ref, size: int ref}

val initialSize = 8

fun nextLength 0 = initialSize
  | nextLength n = 2 * n

fun new () = T {arr = ref (Array.fromList []), size = ref 0}

fun clear (T {arr, size}) = (arr := Array.fromList []; size := 0)

fun push (T {arr = arrRef as ref arr, size}, x) =
   if Array.length arr = !size
      then
         let
            val newLen = nextLength (Array.length arr)
            val newArr = Array.array (newLen, x)
         in
            Array.copy {dst = newArr, src = arr, di = 0}
            ; arrRef := newArr
            ; size := !size + 1
         end
   else
      (Array.update (arr, !size, x)
       ; size := !size + 1)

fun app f (T {arr = ref arr, size = ref size}) =
   let
      fun go i =
         if i < size
            then (f (Array.sub (arr, i)); go (i + 1))
         else ()
   in
      go 0
   end

fun size (T {size, ...}) = !size

fun sub (T {arr = ref arr, size = ref size}, idx) =
   if 0 <= idx andalso idx < size
      then Array.sub (arr, idx)
   else raise Subscript

fun update (T {arr = ref arr, size = ref size}, idx, value) =
   if 0 <= idx andalso idx < size
      then Array.update (arr, idx, value)
   else raise Subscript

fun indexOf p (T {arr = ref arr, size = ref size}) =
   let
      fun go i =
         if i >= size
            then NONE
         else if p (Array.sub (arr, i))
            then SOME i
         else go (i + 1)
   in
      go 0
   end

(* TODO: shrink allocation(?) *)
fun remove p (t as T {arr, size}) =
   case indexOf p t of
      NONE => ()
    | SOME idx =>
         let
            val arr = !arr
            val lim = !size
            fun go i =
               if i + 1 < lim
                  then (Array.update (arr, i, Array.sub (arr, i + 1))
                        ; go (i + 1))
               else ()
         in
            go idx
            ; size := !size - 1
         end

fun findi p (T {arr = ref arr, size = ref size}) =
   let
      fun go i =
         if i >= size
            then NONE
         else if p (i, Array.sub (arr, i))
            then SOME (i, Array.sub (arr, i))
         else go (i + 1)
   in
      go 0
   end

fun reserve (t, newSize, fill) =
   while (size t < newSize) do
      push (t, fill)

fun shrinkToFit (T {arr = arrRef as ref arr, size = ref size}) =
   if Array.length arr = size
      then ()
   else arrRef := Array.tabulate (size, fn i => Array.sub (arr, i))

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
