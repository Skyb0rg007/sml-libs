
structure BurstSort =
struct

datatype 'a node = Node of {
    nullTail : 'a Array.array,
    nullTailIdx : int ref,
    counts : int Array.array,
    ptrs : 'a Array.array
  }


end
