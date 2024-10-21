
structure RecomputeQueue =
struct

structure A = DynamicArray

val priorityHeads: Node.somenode option A.array = A.array (0, NONE)
val count = ref 0

val WARNING_MARK = 250

fun add (node as Node.Node {in_recompute_queue, next_in_recompute_queue, height, ...}) =
   if !in_recompute_queue
      then false
   else
      let
         val () = in_recompute_queue := true
         val () = count := !count + 1
         val priority = !height
      in
         while (priority >= A.bound priorityHeads) do
            (A.update (priorityHeads, A.bound priorityHeads, NONE)
             ; if A.bound priorityHeads = WARNING_MARK
               then TextIO.output (TextIO.stdErr, "Node height reached\n")
               else ())
         ; next_in_recompute_queue := A.sub (priorityHeads, priority)
         ; A.update (priorityHeads, priority, SOME node)
         ; true
      end

fun removeMin () =
   let
      fun go priority =
         if priority >= A.bound priorityHeads
            then NONE
         else
            case A.sub (priorityHeads, priority) of
               NONE => go (priority + 1)
             | SOME (node as Node.Node {in_recompute_queue, next_in_recompute_queue, ...}) =>
                  (in_recompute_queue := false
                   ; A.update (priorityHeads, priority, !next_in_recompute_queue)
                   ; next_in_recompute_queue := NONE
                   ; count := !count - 1
                   ; SOME node)
   in
      go 0
   end

fun drain f =
   while (!count > 0) do
      f (removeMin ())

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
