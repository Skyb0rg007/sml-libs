
structure Node': NODE' = 
struct

fun assert (cond, msg) =
   if cond
      then ()
   else raise Fail ("Assertion failed: " ^ msg)

(* An observer is a callback with an identity *)
structure Observer =
   struct
      datatype t = T of {id: unit ref, func: unit -> unit}

      fun new th = T {id = ref (), func = th}

      fun call (T {func, ...}) = func ()

      fun equals (T {id = id1, ...}, T {id = id2, ...}) = id1 = id2
   end

datatype change = Change | NoChange

(* Node type *)
datatype 'a t = Node of {
      value: 'a option ref,
      untyped: some_node
   }

(* Type-erased node type *)
and some_node = SomeNode of {
      id: unit ref,
      compute: unit -> change,
      dependencies: unit -> some_node list,
      name: string ref,
      height: int ref,
      adjustedHeight: int ref,
      dependents: some_node DynArray.t,
      observers: Observer.t DynArray.t,
      changedAt: int ref,
      inQueue: bool ref,
      nextInQueue: some_node option ref
   }

(* Global variables *)
structure Global =
   struct
      val lastStabilizationNum = ref 0
      val currentStabilizationNum = ref ~1
      val totalRefcount = ref 0
      val queueHeads: some_node option DynArray.t = DynArray.new ()
      val queueCount = ref 0
   end

(* some_node getters and setters *)
structure S =
   struct
      datatype t = datatype some_node

      fun id (SomeNode {id, ...}) = id
      fun compute (SomeNode {compute, ...}) = compute ()
      fun dependencies (SomeNode {dependencies, ...}) = dependencies ()
      fun name (SomeNode {name, ...}) = !name
      fun setName (SomeNode {name, ...}, str) = name := str
      fun height (SomeNode {height, ...}) = !height
      fun setHeight (SomeNode {height, ...}, n) = height := n
      fun adjustedHeight (SomeNode {adjustedHeight, ...}) = !adjustedHeight
      fun setAdjustedHeight (SomeNode {adjustedHeight, ...}, n) = adjustedHeight := n
      fun dependents (SomeNode {dependents, ...}) = dependents
      fun observers (SomeNode {observers, ...}) = observers
      fun changedAt (SomeNode {changedAt, ...}) = !changedAt
      fun setChangedAt (SomeNode {changedAt, ...}, n) = changedAt := n
      fun inQueue (SomeNode {inQueue, ...}) = !inQueue
      fun setInQueue (SomeNode {inQueue, ...}, b) = inQueue := b
      fun nextInQueue (SomeNode {nextInQueue, ...}) = !nextInQueue
      fun setNextInQueue (SomeNode {nextInQueue, ...}, n) = nextInQueue := n
      fun equals (d1, d2) = id d1 = id d2
   end

(* node getters and setters *)
structure N =
   struct
      datatype t = datatype t

      fun toSomeNode (Node {untyped, ...}) = untyped
      fun value (Node {value, ...}) = !value
      fun valueExn n = Option.valOf (value n)
      fun setValue (Node {value, ...}, x) = value := SOME x
      fun equals (n1, n2) = S.equals (toSomeNode n1, toSomeNode n2)
   end

(** Recomputing **)

fun refcount node =
   DynArray.size (S.dependents node) + DynArray.size (S.observers node)

fun addDependent (node, dependent) =
   let
      val oldRefcount = refcount node
   in
      DynArray.push (S.dependents node, dependent)
      ; handleRefcountChange (node, oldRefcount)
   end

and removeDependent (node, dependent) =
   let
      val oldRefcount = refcount node
   in
      DynArray.remove (fn d => S.equals (d, dependent)) (S.dependents node)
      ; handleRefcountChange (node, oldRefcount)
   end

and connect node =
   let
      fun handleDependency dependency =
         let
            val depHeight = S.height dependency
            val ourHeight = S.height node
            val () = addDependent (dependency, node)
         in
            if depHeight + 1 > ourHeight
               then (S.setHeight (node, depHeight + 1)
                     ; S.setAdjustedHeight (node, depHeight + 1))
            else ()
         end
   in
      assert (DynArray.size (S.dependents node) = 0, "connect given node with dependents")
      ; List.app handleDependency (S.dependencies node)
      ; assert (S.compute node = Change, "compute didn't initialize value")
   end

and disconnect node =
   let
      fun handleDependency dependency =
         removeDependent (dependency, node)
   in
      List.app handleDependency (S.dependencies node)
   end

and handleRefcountChange (node, oldRefcount) =
   let
      val newcount = refcount node
   in
      if oldRefcount = 0 andalso newcount > 0
         then connect node
      else if oldRefcount > 0 andalso newcount = 0
         then disconnect node
      else ()
   end

fun addObserver (node, observer) =
   let
      val node = N.toSomeNode node
      val oldRefcount = refcount node
   in
      DynArray.push (S.observers node, observer)
      ; handleRefcountChange (node, oldRefcount)
   end

fun removeObserver (node, observer) =
   let
      val node = N.toSomeNode node
      val oldRefcount = refcount node
   in
      DynArray.remove (fn obs => Observer.equals (obs, observer)) (S.observers node)
      ; handleRefcountChange (node, oldRefcount)
   end

fun ensureHeight (node, newHeight) =
   S.setAdjustedHeight (node, Int.max (S.adjustedHeight node, newHeight))

fun isChangingInCurrentStabilization node =
   S.changedAt node = !Global.currentStabilizationNum

fun enqueue node =
   if S.inQueue node
      then ()
   else
      let
         val priority = S.height node
      in
         S.setInQueue (node, true)
         ; Global.queueCount := !Global.queueCount + 1
         ; if priority >= DynArray.size Global.queueHeads
              then DynArray.resize (Global.queueHeads, priority + 1, NONE)
           else ()
         ; S.setNextInQueue (node, DynArray.sub (Global.queueHeads, priority))
         ; DynArray.update (Global.queueHeads, priority, SOME node)
      end

fun removeMin () =
   case DynArray.findi (fn (_, n) => Option.isSome n) Global.queueHeads of
      SOME (priority, SOME node) =>
         (S.setInQueue (node, false)
          ; DynArray.update (Global.queueHeads, priority, S.nextInQueue node)
          ; S.setNextInQueue (node, NONE)
          ; Global.queueCount := !Global.queueCount - 1
          ; node)
    | _ => raise Fail "removeMin: empty recompute queue!"

fun recomputeNode node =
   let
      val height = S.height node
      val adjustedHeight = S.adjustedHeight node
   in
      if adjustedHeight > height
         then
            (DynArray.app
               (fn dependent => ensureHeight (dependent, adjustedHeight + 1))
               (S.dependents node)
             ; S.setHeight (node, adjustedHeight)
             ; enqueue node)
      else
         case S.compute node of
            NoChange => ()
          | Change =>
               (S.setChangedAt (node, !Global.currentStabilizationNum)
                ; DynArray.app enqueue (S.dependents node)
                ; DynArray.app Observer.call (S.observers node))
   end

fun stabilize () =
   let
      val oldStabilizationNum = !Global.lastStabilizationNum
      val currentStabilizationNum = oldStabilizationNum + 1
   in
      Global.lastStabilizationNum := currentStabilizationNum
      ; Global.currentStabilizationNum := currentStabilizationNum
      ; while !Global.queueCount > 0 do recomputeNode (removeMin ())
      ; Global.currentStabilizationNum := ~1
   end

(** Node primitives **)

fun create {compute, dependencies} =
   let
      val value = ref NONE
      val untyped = SomeNode {
            id = ref (),
            compute = fn () =>
               case compute (!value) of
                  NONE => NoChange
                | SOME x => (value := SOME x; Change),
            dependencies = dependencies,
            name = ref "",
            height = ref 0,
            adjustedHeight = ref 0,
            dependents = DynArray.new (),
            observers = DynArray.new (),
            changedAt = ref ~2,
            inQueue = ref false,
            nextInQueue = ref NONE
         }
   in
      Node {value = value, untyped = untyped}
   end

fun constant x = create
   {compute = fn _ => SOME x,
    dependencies = fn () => []}

fun map f node =
   let
      val deps = [N.toSomeNode node]
      fun compute _ = SOME (f (N.valueExn node))
   in
      create {compute = compute, dependencies = fn () => deps}
   end

fun mapOptional f node =
   let
      val deps = [N.toSomeNode node]
      fun compute _ =
         case N.value node of
            NONE => NONE
          | SOME x => f x
   in
      create {compute = compute, dependencies = fn () => deps}
   end

fun map2 f node1 node2 =
   let
      val deps = [N.toSomeNode node1, N.toSomeNode node2]
      fun compute _ = SOME (f (N.valueExn node1, N.valueExn node2))
   in
      create {compute = compute, dependencies = fn () => deps}
   end

fun switch {alwaysFire} (lhs: 'a t) (k: 'a -> 'b t): 'b t =
   let
      val mainNodeRef = ref NONE
      val rhsNode = create
         {compute = fn oldRhsOpt =>
            let
               val rhs = k (N.valueExn lhs)
               val main = Option.valOf (!mainNodeRef)
               val () = addDependent (N.toSomeNode rhs, N.toSomeNode main)
               val depHeight = S.height (N.toSomeNode rhs)
               val () = ensureHeight (N.toSomeNode main, depHeight + 1)
               val () =
                  case oldRhsOpt of
                     NONE => ()
                   | SOME oldRhs => removeDependent (N.toSomeNode oldRhs, N.toSomeNode main)
            in
               SOME rhs
            end,
          dependencies = fn () => [N.toSomeNode lhs]}

      val main = create
         {compute = fn _ =>
            let
               val rhs = N.valueExn rhsNode
            in
               if alwaysFire orelse isChangingInCurrentStabilization (N.toSomeNode rhs)
                  then N.value rhs
               else NONE
            end,
          dependencies = fn () => [N.toSomeNode rhsNode]}
   in
      main
   end

fun bind x k = switch {alwaysFire = true} x k

fun fold f z node = create
   {compute = fn oldValue =>
      let
         val state = Option.getOpt (oldValue, z)
      in
         if isChangingInCurrentStabilization (N.toSomeNode node)
            then SOME (f (N.valueExn node, state))
         else SOME state
      end,
    dependencies = fn () => [N.toSomeNode node]}

fun leftmost inputs = create
   {compute = fn _ =>
      let
         fun go [] = NONE
           | go (input :: inputs) =
            if isChangingInCurrentStabilization (N.toSomeNode input)
               then N.value input
            else go inputs
      in
         go inputs
      end,
    dependencies = fn () => List.map N.toSomeNode inputs}

datatype 'a event = Event of 'a t

fun readEvent (Event node) = node
fun newEvent () = Event (create {compute = fn x => x, dependencies = fn () => []})
fun triggerEvent (Event node, x) =
   (N.setValue (node, x)
    ; enqueue (N.toSomeNode node))

datatype 'a var = Var of 'a t

fun readVar (Var node) = node
fun newVar x =
   let
      val node = create
         {compute = fn x => SOME (Option.valOf x),
          dependencies = fn () => []}
   in
      N.setValue (node, x)
      ; Var node
   end
fun setVar (Var node, x) =
   (N.setValue (node, x)
    ; enqueue (N.toSomeNode node))

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
