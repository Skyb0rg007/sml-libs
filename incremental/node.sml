
structure Node: NODE =
struct

val connectData = Profile.Data.malloc ()
val disconnectData = Profile.Data.malloc ()
val stabilizeData = Profile.Data.malloc ()
val bumpHeightData = Profile.Data.malloc ()
val computeData = Profile.Data.malloc ()

datatype observer = Observer of {id: unit ref, func: unit -> unit}

datatype t = T of {
      id: unit ref,
      compute: unit -> bool,
      dependencies: unit -> t list,
      name: string ref,
      height: int ref,
      adjustedHeight: int ref,
      dependents: t DynArray.t,
      observers: observer DynArray.t,
      changedAt: int ref,
      inQueue: bool ref,
      nextInQueue: t option ref
   }

datatype subscription = Subscription of {id: unit ref, node: t}

(* Globals *)
val lastStabilizationNum = ref 0
val currentStabilizationNum = ref ~1
val totalRefcount = ref 0
val recomputeQueue: t option DynArray.t = DynArray.new ()
val recomputeCount = ref 0

fun new {compute, dependencies} =
   T {
      id = ref (),
      compute = compute,
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

fun equals (T {id = id1, ...}, T {id = id2, ...}) = id1 = id2

fun name (T {name, ...}) = !name
fun setName (T {name, ...}, str) = name := str

fun refcount (T {dependents, observers, ...}) =
   DynArray.size dependents + DynArray.size observers

(* Add `dependent` to `node`s dependencies *)
fun addDependent (node as T {dependents, ...}, dependent) =
   let
      val oldRefcount = refcount node
   in
      DynArray.push (dependents, dependent)
      ; handleRefcountChange (node, oldRefcount)
   end

(* Remove `dependent` from `node`s dependencies *)
and removeDependent (node as T {dependents, ...}, dependent) =
   let
      val oldRefcount = refcount node
   in
      DynArray.remove (fn d => equals (d, dependent)) dependents
      ; handleRefcountChange (node, oldRefcount)
   end

(* Connects or disconnects `node` based on its refcount *)
and handleRefcountChange (node, oldRefcount) =
   let
      val newRefcount = refcount node
   in
      if oldRefcount = 0 andalso newRefcount > 0
         then connect node
      else if oldRefcount > 0 andalso newRefcount = 0
         then disconnect node
      else ()
      ; totalRefcount := !totalRefcount - oldRefcount + newRefcount
   end

(* Adds the node to the graph, updating dependency heights *)
and connect node =
      Profile.withData (connectData, fn () =>
         let
            val T {height, adjustedHeight, dependencies, compute, ...} = node
            fun handleDep (dependency as T {height = ref depHeight, ...}) =
               (addDependent (dependency, node)
               ; if depHeight + 1 > !height
                    then (height := depHeight + 1; adjustedHeight := depHeight + 1)
                 else ())
         in
            List.app handleDep (dependencies ())
            ; ignore (compute ())
         end)

(* Removes the node to the graph *)
and disconnect (node as T {dependencies, ...}) =
   Profile.withData (disconnectData, fn () =>
      List.app
         (fn dependency => removeDependent (dependency, node))
         (dependencies ()))

(* Adds the given callback to the node's observer list *)
fun subscribe (node as T {observers, ...}) func =
   let
      val id = ref ()
      val obs = Observer {id = id, func = func}
      val oldRefcount = refcount node
   in
      DynArray.push (observers, obs)
      ; handleRefcountChange (node, oldRefcount)
      ; Subscription {id = id, node = node}
   end

(* Removes a callback to the node's observer list *)
fun unsubscribe (Subscription {id, node as T {observers, ...}}) =
   let
      val oldRefcount = refcount node
   in
      DynArray.remove (fn Observer {id = id', ...} => id = id') observers
      ; handleRefcountChange (node, oldRefcount)
   end

(* Updates the `adjustedHeight`.
 * This will update `height` during the next stabilization *)
fun ensureHeight (T {adjustedHeight, ...}, newHeight) =
   adjustedHeight := Int.max (!adjustedHeight, newHeight)

(* Has the node been updated during the current stabilization *)
fun isChanging (T {changedAt, ...}) =
   !changedAt = !currentStabilizationNum

(* Add the node to the recomputation priority queue *)
fun enqueue (node as T {inQueue, nextInQueue, height = ref priority, ...}) =
   if !inQueue
      then ()
   else
      (inQueue := true
       ; recomputeCount := !recomputeCount + 1
       ; DynArray.reserve (recomputeQueue, priority + 1, NONE)
       ; nextInQueue := DynArray.sub (recomputeQueue, priority)
       ; DynArray.update (recomputeQueue, priority, SOME node))

(* Remove the node with the highest (smallest) priority from the queue *)
fun removeMin () =
   case DynArray.findi (fn (_, n) => Option.isSome n) recomputeQueue of
      SOME (priority, SOME (node as T {inQueue, nextInQueue, ...})) =>
         (inQueue := false
          ; DynArray.update (recomputeQueue, priority, !nextInQueue)
          ; nextInQueue := NONE
          ; recomputeCount := !recomputeCount - 1
          ; node)
    | _ => raise Fail "Attempt to dequeue from empty recompute queue"

(* Update the node.
 * If its height was adjusted, update its height and re-queue it
 * Otherwise, update it and notify its dependencies *)
fun recomputeNode node =
   let
      val T {height, adjustedHeight, dependents,
             observers, compute, changedAt, ...} = node
   in
      if !adjustedHeight > !height
         then
            Profile.withData (bumpHeightData, fn () =>
               (DynArray.app
                     (fn dependent => ensureHeight (dependent, !adjustedHeight + 1))
                     dependents
                ; height := !adjustedHeight
                ; enqueue node))
      else
         Profile.withData (computeData, fn () =>
            if compute ()
               then
                  (changedAt := !currentStabilizationNum
                   ; DynArray.app enqueue dependents
                   ; DynArray.app (fn Observer {func, ...} => func ()) observers)
            else ())
   end

(* Run a stabilization *)
fun stabilize () =
   Profile.withData (stabilizeData, fn () =>
      let
         val oldStab = !lastStabilizationNum
         val curStab = oldStab + 1
      in
         lastStabilizationNum := curStab
         ; currentStabilizationNum := curStab
         ; while !recomputeCount > 0 do
              recomputeNode (removeMin ())
         ; currentStabilizationNum := ~1
      end)

(** Preparing for export **)

val dirty = enqueue

(* A node should be updated before its dependencies *)
val addDependent = fn (node as T {height, ...}, dependent) =>
   (addDependent (node, dependent)
    ; ensureHeight (dependent, !height + 1))

fun touch node =
   if refcount node = 0
      then (connect node; handleRefcountChange (node, 1))
   else ()

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
