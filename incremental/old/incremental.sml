
structure Incremental =
struct

structure U = Universal

structure Global =
   struct
      val lastStabilizationNum = ref 0
      val currentStabilizationNum = ref ~1
      val totalRefcount = ref 0
   end

fun isChangingInCurrentStabilization node =
   Node.changedAt node = !Global.currentStabilizationNum

(** Adding and removing dependencies **)

fun addDependent (node, dependent) =
   let
      val oldRefcount = Node.refcount node
      val dependents = Node.dependents node
   in
      DynArray.push (dependents, dependent)
      ; handleRefcountChange (node, oldRefcount)
   end

and removeDependent (node, dependent) =
   let
      val oldRefcount = Node.refcount node
      val dependents = Node.dependents node
   in
      DynArray.remove (fn d => Node.equals (d, dependent)) dependents
      ; handleRefcountChange (node, oldRefcount)
   end

and handleRefcountChange (node, oldRefcount) =
   let
      val newRefcount = Node.refcount node
   in
      if oldRefcount = 0 andalso newRefcount > 0
         then connect node
      else if oldRefcount > 0 andalso newRefcount = 0
         then disconnect node
      else ()
      ; Global.totalRefcount := !Global.totalRefcount - oldRefcount + newRefcount
   end

and connect node =
   (List.app
      (fn dependency =>
         let
            val () = addDependent (dependency, node)
            val depHeight = Node.height dependency
            val ourHeight = Node.height node
         in
            if depHeight + 1 > ourHeight
               then
                  (Node.setHeight (node, depHeight + 1)
                   ; Node.setAdjustedHeight (node, depHeight + 1))
            else ()
         end)
      (Node.dependencies node)
    ; Node.setValue (node, Node.compute node))

and disconnect node =
   (List.app
      (fn dependency => removeDependent (dependency, node))
      (Node.dependencies node))

(** Adding and removing observers **)

fun addObserver (node, observer) =
   let
      val oldRefcount = Node.refcount node
      val observers = Node.observers node
   in
      DynArray.push (observers, observer)
      ; handleRefcountChange (node, oldRefcount)
   end

fun removeObserver (node, observer) =
   let
      val oldRefcount = Node.refcount node
      val observers = Node.observers node
   in
      DynArray.remove (fn obs => Observer.equals (obs, observer)) observers
      ; handleRefcountChange (node, oldRefcount)
   end

(** Recomputing **)

fun ensureHeight (node, newHeight) =
   Node.setAdjustedHeight (node, Int.max (Node.adjustedHeight node, newHeight))

fun recomputeNode node =
   let
      val height = Node.height node
      val adjustedHeight = Node.adjustedHeight node
   in
      if adjustedHeight > height
         then
            (DynArray.app
                (fn dependent => ensureHeight (dependent, adjustedHeight + 1))
                (Node.dependents node)
             ; Node.setHeight (node, adjustedHeight)
             ; ignore (Node.PQ.add node))
      else
         let
            val newValue = Node.compute node
         in
            if Node.isNone newValue
               then ()
            else
               (Node.setValue (node, newValue)
                ; Node.setChangedAt (node, !Global.currentStabilizationNum)
                ; DynArray.app
                     (fn dependent => ignore (Node.PQ.add dependent))
                     (Node.dependents node)
                ; DynArray.app
                     (fn observer => Observer.observe (observer, newValue))
                     (Node.observers node))
         end
   end

fun stabilize () =
   let
      val oldStabilizationNum = !Global.lastStabilizationNum
      val currentStabilizationNum = oldStabilizationNum + 1
   in
      Global.lastStabilizationNum := currentStabilizationNum
      ; Global.currentStabilizationNum := currentStabilizationNum
      ; Node.PQ.drain recomputeNode
      ; Global.currentStabilizationNum := ~1
   end

(** Computational nodes **)

fun constant x = Node.create
   {compute = fn _ => x,
    dependencies = fn () => []}

fun map f n = Node.create
   {compute = fn _ => f (Node.valueExn n),
    dependencies = fn () => [n]}

fun mapOptional f n = Node.create
   {compute = fn _ =>
      let
         val a = Node.value n
      in
         if Node.isNone a
            then Node.none
         else f a
      end,
    dependencies = fn () => [n]}

fun map2 f n1 n2 = Node.create
   {compute = fn _ =>
      let
         val a = Node.valueExn n1
         val b = Node.valueExn n2
      in
         f (a, b)
      end,
    dependencies = fn () => [n1, n2]}

fun switch {alwaysFire} (lhs: Node.t) (k: U.universal -> Node.t) =
   let
      val nodeTag: Node.t U.tag = U.tag ()
      val mainNodeRef: Node.t option ref = ref NONE
      val rhsNode = Node.create
         {compute = fn node =>
            let
               val lhsVal = Node.valueExn lhs
               val rhs = k lhsVal
               val main = Option.valOf (!mainNodeRef)
               val () = addDependent (rhs, main)
               val () = ensureHeight (main, Node.height rhs + 1)
               val oldRhs = Node.value node
            in
               if Node.isNone oldRhs
                  then ()
               else removeDependent (U.tagProject nodeTag oldRhs, main)
               ; U.tagInject nodeTag rhs
            end,
          dependencies = fn () => [lhs]}
      val () = Node.setName (rhsNode, "switch data")

      val main = Node.create
         {compute = fn _ =>
            let
               val rhs = U.tagProject nodeTag (Node.valueExn rhsNode)
            in
               if alwaysFire orelse isChangingInCurrentStabilization rhs then
                  Node.value rhs
               else Node.none
            end,
          dependencies = fn () =>
            let
               val rhsOpt = Node.value rhsNode
            in
               if Node.isNone rhsOpt
                  then [rhsNode]
               else [rhsNode, U.tagProject nodeTag rhsOpt]
            end}
   in
      mainNodeRef := SOME main
      ; main
   end

fun bind lhs k = switch {alwaysFire = true} lhs k

fun leftmost inputs = Node.create
   {compute = fn _ =>
      let
         fun go [] = Node.none
           | go (input :: inputs) =
            if isChangingInCurrentStabilization input
                  andalso not (Node.isNone (Node.value input))
               then Node.value input
            else go inputs
      in
         go inputs
      end,
    dependencies = fn () => inputs}

fun sample f signal clock = Node.create
   {compute = fn _ =>
      if isChangingInCurrentStabilization clock
         then f (Node.valueExn signal, Node.valueExn clock)
      else Node.none,
    dependencies = fn () => [signal, clock]}

fun fold f initial a = Node.create
   {compute = fn node =>
      let
         val stateOpt = Node.value node
         val state = if Node.isNone stateOpt then initial else stateOpt
      in
         if isChangingInCurrentStabilization a
            then f (Node.valueExn a, state)
         else state
      end,
    dependencies = fn () => [a]}

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
