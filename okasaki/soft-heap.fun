
functor SoftHeapFn(T : ORDERED) : SOFT_HEAP =
struct
   type elt = T.t

   datatype node = Node of T.t * int * node * node * elt list

   datatype t = Heap of node * t * t * t * int
end

(* vim: set ts=3 sw=3: *)
