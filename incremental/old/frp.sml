
structure FRP: FRP =
struct

structure U = Universal
structure I = Incremental

datatype 'a event = E of Node.t * 'a U.tag
datatype 'a behavior = B of Node.t * 'a U.tag
datatype 'a dynamic = D of Node.t * 'a U.tag

structure E =
   struct
      type 'a t = 'a event

      val neverNode = Node.create
         {compute = fn node => Node.value node,
          dependencies = fn () => []}

      fun never () = E (neverNode, U.tag ())

      fun leftmost events =
         let
            val n = I.leftmost (List.map (fn E (n, _) => n) events)
         in
            (* TODO *)
            E (n, U.tag ())
         end

      fun map f (E (node, tag)) =
         let
            val tag' = U.tag ()
            fun f' x = U.tagInject tag' (f (U.tagProject tag x))
            val node' = I.mapOptional f' node
         in
            Node.setName (node', "E.map")
            ; E (node', tag')
         end

      fun new () =
         let
            val tag = U.tag ()
            val node = Node.create
               {compute = Node.value,
                dependencies = fn () => []}
            fun fire x =
               (Node.setValue (node, U.tagInject tag x)
                ; Node.PQ.add node
                ; I.stabilize ())
         in
            (E (node, tag), fire)
         end
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
