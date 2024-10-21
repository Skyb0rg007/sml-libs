
functor HeapFn(S:
   sig
      type t

      val <= : t * t -> bool
   end): HEAP =
struct

type item = S.t

datatype t = T of {
      arr: item option array ref,
      size: int ref
   }

fun size (T {size, ...}) = !size

fun isEmpty (T {size, ...}) = !size = 0

fun new () =
   T {arr = ref (Array.array (64, NONE)),
      size = ref 0}

fun peek (T {arr, size}) = Array.sub (!arr, 0)

fun sift_up (T {arr, ...}, start, pos) =
   let
      val arr = !arr

      fun go (pos, item) =
         let
            val parentIdx = (pos - 1) div 2
            val parentItem = Option.valOf (Array.sub (arr, parentIdx))
         in
            if S.<= (item, parentItem)
               then ()
               else
                  (Array.update (arr, parentIdx, SOME item);
                   if parentIdx > start
                     then go (parentIdx, parentItem)
                     else ())
         end
   in
      go (pos, Option.valOf (Array.sub (arr, pos)))
   end

fun sift_down_to_bottom (T {arr, size}, pos) =
   let
      val endIdx = !size
      val startIdx = pos;

      fun go (pos, item, childPos) =
         (* if childPos <= endIdx - 2 *)
         let
            val lchild = 2 * pos + 1
            val rchild = lchild + 1
         in
            raise Fail "NYI"
         end
   in
      raise Fail "NYI"
   end

fun push (h as T {arr, size}, x) =
   (if Array.length (!arr) = !size
      then
         let
            val newCap = !size * 2
            val newArr = Array.array (newCap, NONE)
         in
            Array.copy {dst = newArr, src = !arr, di = 0};
            arr := newArr
         end
      else ();
    Array.update (!arr, !size, SOME x);
    size := !size + 1;
    sift_up (h, 0, !size - 1))

fun pop (h as T {arr, size}) =
   case Array.sub (!arr, 0) of
      NONE => NONE
    | SOME item =>
         if !size = 1
            then (size := 0; SOME item)
         else sift_down_to_bottom (h, 0)

end

(* vim: set tw=0 ts=3 sw=3: *)
