
structure Node =
struct

structure U = Universal

(* Dynamic array
 * Needed since DynamicArray requires a default element *)
structure DynArray:
   sig
      type 'a t

      val new: unit -> 'a t
      val size: 'a t -> int
      val push: 'a t * 'a -> unit
      val sub: 'a t * int -> 'a
      val update: 'a t * int * 'a -> unit
      val remove: ('a -> bool) -> 'a t -> unit
      val app: ('a -> unit) -> 'a t -> unit
   end =
   struct
      datatype 'a t = DynArray of {
            data: 'a array option ref,
            size: int ref
         }

      fun new () = DynArray {data = ref NONE, size = ref 0}

      fun size (DynArray {size, ...}) = !size

      fun push (DynArray {data, size}, x) =
         case !data of
            NONE => (data := SOME (Array.array (8, x)); size := 1)
          | SOME arr =>
               if Array.length arr = !size
                  then
                     let
                        val newArr = Array.array (2 * !size, x)
                     in
                        Array.copy {src = arr, dst = newArr, di = 0}
                        ; data := SOME newArr
                        ; size := !size + 1
                     end
               else (Array.update (arr, !size, x); size := !size + 1)

      fun sub (DynArray {data, size}, idx) =
         if idx < !size
            then Array.sub (Option.valOf (!data), idx)
         else raise Subscript

      fun update (DynArray {data, size}, idx, value) =
         if idx < !size
            then Array.update (Option.valOf (!data), idx, value)
         else raise Subscript

      fun app f (DynArray {data, size}) =
         case !data of
            NONE => ()
          | SOME arr =>
               let
                  val size = !size
                  fun go i =
                     if i < size
                        then (f (Array.sub (arr, i)); go (i + 1))
                     else ()
               in
                  go 0
               end

      fun toList (DynArray {data, size}) =
         case !data of
            NONE => []
          | SOME arr => List.tabulate (!size, fn i => Array.sub (arr, i))

      fun remove p (DynArray {data, size}) =
         case !data of
            NONE => ()
          | SOME arr =>
               let
                  fun shiftFrom i =
                     if i + 1 < !size
                        then (Array.update (arr, i, Array.sub (arr, i + 1))
                              ; shiftFrom (i + 1))
                     else ()

                  fun go i =
                     if i >= !size
                        then ()
                     else
                        if p (Array.sub (arr, i))
                           then (shiftFrom i; size := !size - 1)
                        else go (i + 1)
               in
                  go 0
               end
   end

(* Since the `value` field is `universal`,
 * we can make it optional via a unique tag *)
val noneTag: unit U.tag = U.tag ()
val none = U.tagInject noneTag ()
val isNone = U.tagIs noneTag

datatype observer = Observer of {
      id: unit ref,
      onChange: U.universal -> unit
   }

datatype node = Node of {
      (* How to compute the value. Returns none if no change occurred. *)
      compute: node -> U.universal,
      (* Compute the dependency list *)
      dependencies: unit -> node list,
      (* Nodes that depend on this node's value *)
      dependents: node DynArray.t,
      (* Functions that should be called when this node's value changes *)
      observers: observer DynArray.t,
      (* Contains the value, or `none` if it hasn't been set yet *)
      value: U.universal ref,
      (* *)
      height: int ref,
      adjustedHeight: int ref,
      (* For intrusive priority queue implementation *)
      inRecomputeQueue: bool ref,
      nextInRecomputeQueue: node option ref,
      (* Name for debugging output *)
      name: string ref,
      (* Most recent stabilization number *)
      changedAt: int ref
   }

fun create {compute, dependencies} = Node {
      compute = compute,
      dependencies = dependencies,
      dependents = DynArray.new (),
      observers = DynArray.new (),
      value = ref none,
      height = ref 0,
      adjustedHeight = ref 0,
      inRecomputeQueue = ref false,
      nextInRecomputeQueue = ref NONE,
      name = ref "",
      changedAt = ref ~2
   }

fun sameNode (Node {name = n1, ...}, Node {name = n2, ...}) = n1 = n2

fun refcount (Node {observers, dependents, ...}) =
   let
      val numObservers = DynArray.size observers
      val numDependents = DynArray.size dependents
   in
      numObservers + numDependents
   end

fun value (Node {value = ref value, ...}) =
   if isNone value
      then raise Fail "someValue: none"
   else value

fun setValue (Node {value, ...}, x) =
   value := x

fun annotate (Node {name, ...}, str) = name := str

fun name (Node {name, ...}) = !name

fun isChangingInCurrentStabilization (Node {changedAt, ...}) =
   !changedAt = !Globals.currentStabilizationNum

(* Recompute Queue *)
structure Q =
   struct
      (* Use DynamicArray over DynArray since theres a good default element *)
      val heads: node option DynamicArray.array = DynamicArray.array (0, NONE)
      val count = ref 0
      
      fun add (node as Node {inRecomputeQueue, nextInRecomputeQueue, height = ref priority, ...}) =
         if !inRecomputeQueue
            then false
         else
            (inRecomputeQueue := true
             ; count := !count + 1
             ; nextInRecomputeQueue := DynamicArray.sub (heads, priority)
             ; DynamicArray.update (heads, priority, SOME node)
             ; true)

      fun removeMin () =
         let
            fun go i =
               if i < DynamicArray.bound heads
                  then
                     case DynamicArray.sub (heads, i) of
                        NONE => go (i + 1)
                      | SOME (node as Node {inRecomputeQueue, nextInRecomputeQueue, ...}) =>
                           (inRecomputeQueue := false
                            ; DynamicArray.update (heads, i, !nextInRecomputeQueue)
                            ; nextInRecomputeQueue := NONE
                            ; count := !count - 1
                            ; SOME node)
               else NONE
         in
            go 0
         end

      fun drain f =
         while !count > 0 do
            f (Option.valOf (removeMin ()))
   end

fun addDependent (node as Node {dependents, ...}, dependent) =
   let
      val oldRefcount = refcount node
   in
      DynArray.push (dependents, dependent)
      ; handleRefcountChange (node, oldRefcount)
   end

and removeDependent (node as Node {dependents, ...}, dependent) =
   let
      val oldRefcount = refcount node
   in
      DynArray.remove (fn d => sameNode (d, dependent)) dependents
      ; handleRefcountChange (node, oldRefcount)
   end

and connect (node as Node {compute, dependencies, height, adjustedHeight, ...}) =
   (List.app
       (fn dependency as Node {height = depHeight, ...} =>
          let
             val () = addDependent (dependency, node)
             val depHeight = !depHeight
          in
             if depHeight + 1 > !height
                then
                   (height := depHeight + 1; adjustedHeight := depHeight + 1)
             else ()
          end)
       (dependencies ())
    ; setValue (node, compute node))

and disconnect (node as Node {dependencies, ...}) =
   List.app
      (fn dependency =>
         removeDependent (dependency, node))
      (dependencies ())

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

fun addObserver (node as Node {observers, ...}) observer =
   let
      val oldRefcount = refcount node
   in
      DynArray.push (observers, observer)
      ; handleRefcountChange (node, oldRefcount)
   end

fun removeObserver (node as Node {observers, ...}) (Observer {id, ...}) =
   let
      val oldRefcount = refcount node
   in
      DynArray.remove (fn Observer {id = id', ...} => id = id') observers
      ; handleRefcountChange (node, oldRefcount)
   end


fun ensureHeight (Node {adjustedHeight, ...}, newHeight) =
   adjustedHeight := Int.max (!adjustedHeight, newHeight)

fun recomputeNode (node as Node {height, adjustedHeight, observers, dependents, compute, changedAt, ...}) =
   if !adjustedHeight > !height
      then
         (DynArray.app
            (fn dependent => ensureHeight (dependent, !adjustedHeight + 1))
            dependents
          ; height := !adjustedHeight
          ; ignore (Q.add node))
   else
      let
         val newValue = compute node
      in
         if isNone newValue
            then ()
         else
            (setValue (node, newValue)
             ; changedAt := !Globals.currentStabilizationNum
             ; DynArray.app
                  (fn dependent =>
                     ignore (Q.add dependent))
                  dependents
             ; DynArray.app
                  (fn Observer {onChange, ...} =>
                     onChange newValue)
                  observers)
      end

fun stabilize () =
   let
      val oldStabilizationNum = !Globals.lastStabilizationNum
      val currentStabilizationNum = oldStabilizationNum + 1
   in
      Globals.lastStabilizationNum := currentStabilizationNum
      ; Globals.currentStabilizationNum := currentStabilizationNum
      ; Q.drain recomputeNode
      ; Globals.currentStabilizationNum := ~1
   end

fun constant x =
   create {compute = fn _ => x, dependencies = fn () => []}

fun switch {alwaysFile} lhs f =
   let
      val mainNode = ref NONE
      val rhsNode = create
         {compute = fn node =>
            let
               val rhs as Node {height = rhsHeight, ...} = f (value lhs)
               val main = Option.valOf (!mainNode)
            in
               addDependent (rhs, main)
               ; ensureHeight (main, !rhsHeight + 1)
               ;
               none
            end,
          dependencies = fn () => [lhs]}
   in
      ()
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
