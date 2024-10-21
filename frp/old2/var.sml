
structure Var =
struct

datatype 'a t = Var of 'a Node.node

fun new value =
   let
      val node = Node.create
         {compute = fn node => SOME (Node.value node),
          dependencies = fn () => []}
   in
      Node.setValue (node, value)
      ; Var node
   end

fun set (Var node, value) =
   (Node.setValue (node, value)
    ; Node.Q.add (Node.toSomeNode node))

fun read (Var node) = node

fun constant x = Node.create
   {compute = fn _ => SOME x,
    dependencies = fn () => []}

fun map f n = Node.create
   {compute = fn _ => SOME (f (Node.value n)),
    dependencies = fn () => [Node.toSomeNode n]}

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
