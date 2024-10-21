
structure RealTimeDeque : DEQUE =
struct

val c = 2

datatype 'a t = Queue of 'a list * int * 'a list * 'a list * int * 'a list

exception Empty

val empty = Queue ([], 0, [], [], 0, [])

fun isEmpty (Queue (_, lenF, _, _, lenR, _)) = lenF = 0 andalso lenR = 0

fun rotateRev ([], f, a) = List.rev f @ a
  | rotateRev (x :: r, f, a) = x :: rotateRev (r, List.drop (f, c), List.rev (List.take (f, c)) @ a)

fun rotateDrop (r, i, f) =
   if i < c
      then rotateRev (r, List.drop (f, i), [])
   else
      List.hd r :: rotateDrop (List.tl r, i - c, List.drop (f, c))

fun exec1 (x :: s) = s
  | exec1 s = s

fun exec2 s = exec1 (exec1 s)

fun queue (f, lenF, sf, r, lenR, sr) =
   if lenF > c * lenR + 1
      then
         let
            val i = (lenF + lenR) div 2
            val j = lenF + lenR - i
            val f' = List.take (f, i)
            val r' = rotateDrop (r, i, f)
         in
            Queue (f', i, f', r', j, r')
         end
   else if lenR > c * lenF + 1
      then
         let
            val i = (lenF + lenR) div 2
            val j = lenF + lenR - i
            val f' = rotateDrop (f, j, r)
            val r' = List.take (r, j)
         in
            Queue (f', i, f', r', j, r')
         end
   else
      Queue (f, lenF, sf, r, lenR, sr)

fun cons (x, Queue (f, lf, sf, r, lr, sr)) =
   queue (x :: f, lf + 1, exec1 sf, r, lr, sr)

fun head (Queue ([], _, _, [], _, _)) = raise Empty
  | head (Queue ([], _, _, x :: _, _, _)) = x
  | head (Queue (x :: _, _, _, _, _, _)) = x

fun tail (Queue ([], _, _, [], _, _)) = raise Empty
  | tail (Queue ([], _, _, _ :: _, _, _)) = empty
  | tail (Queue (x :: f, lf, sf, r, lr, sr)) =
   queue (f, lf - 1, exec2 sf, r, lr, exec2 sr)

fun snoc (Queue (f, lf, sf, r, lr, sr), x) =
   queue (f, lf, sf, x :: r, lr + 1, exec1 sr)

fun last (Queue ([], _, _, [], _, _)) = raise Empty
  | last (Queue (x :: _, _, _, [], _, _)) = x
  | last (Queue (_, _, _, x :: _, _, _)) = x

fun init (Queue ([], _, _, [], _, _)) = raise Empty
  | init (Queue (_ :: _, _, _, [], _, _)) = empty
  | init (Queue (f, lf, sf, x :: r, lr, sr)) =
   queue (f, lf, exec2 sf, r, lr - 1, exec2 sr)

fun toList q =
   let
      fun go (q, acc) =
         if isEmpty q
            then acc
         else go (init q, last q :: acc)
   in
      go (q, [])
   end

end

(* vim: set ts=3 sw=3: *)
