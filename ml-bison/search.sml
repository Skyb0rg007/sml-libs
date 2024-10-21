
functor AStar(State : HASH_KEY) =
struct

structure Tbl = HashTableFn(State)

type state = State.hash_key

fun astar start neighbors merge success = []

type neighbors = {explore_all : bool, state : state} -> (int * int * state) list
type merge = state * state -> state
type success = state -> bool

val _ : state -> neighbors -> merge -> success -> state list = astar

end
