
signature NODE =
sig

type t
type subscription

(* `new {compute, dependencies}`
 * Create a new node in the incremental computation graph
 * `compute ()` updates the node. Propogates the change if it returns true.
 * `dependencies ()` returns the nodes whose values are used in `compute`
 * `dependencies` is called when the node is connected/disconnected from the graph *)
val new: {compute: unit -> bool, dependencies: unit -> t list} -> t

(* Returns true if the given node is updated in the current stabilization
 * This function only returns true in a stabilization cycle,
 * so it only makes sense to call inside `compute` *)
val isChanging: t -> bool

(* `addDependent (node, dependent)` adds `dependent` to `node`s dependencies
 * This must be manually called if a dependency is added after a node
 * is connected *)
val addDependent: t * t -> unit

(* `removeDependent (node, dependent)` removes `dependent` from `node`s dependencies
 * This must be manually called if a dependency is removed before the node
 * is disconnected *)
val removeDependent: t * t -> unit

(* Debugging info *)
val setName: t * string -> unit
val name: t -> string

(* `dirty node` marks the node's value as changed,
 * and the value is recomputed during the next stabilization *)
val dirty: t -> unit

(* `subscribe n k`
 * Registers the callback `k` to be called whenever the node `n` is modified *)
val subscribe: t -> (unit -> unit) -> subscription

(* `unsubscribe s`
 * Removes a previously registered callback from the system *)
val unsubscribe: subscription -> unit

(* `touch node`
 * Momentarily connects the node to the recomputation graph
 * Nodes without dependents or subscribers are not updated,
 * so by touch-ing a node we ensure that it is up-to-date *)
val touch: t -> unit

(* `stabilize ()`
 * Updates the system, recomputing all dirty nodes *)
val stabilize: unit -> unit

(* `equals (n1, n2)`
 * Returns true if the two nodes are the same *)
val equals: t * t -> bool

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
