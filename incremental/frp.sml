
structure FRP: FRP =
struct

infixr 9 >>=
infix 3 ::=

val onCleanup: Node.subscription DynArray.t = DynArray.new ()

fun cleanup () =
   (DynArray.app Node.unsubscribe onCleanup
    ; DynArray.clear onCleanup)

structure Event =
   struct
      datatype 'a t = Never
                    | Event of {value: 'a option ref, node: Node.t}

      val never = Never

      fun map _ Never = Never
        | map f (Event {value, node}) =
         let
            val value' = ref NONE
            val node' = Node.new
               {compute = fn () =>
                  case !value of
                     NONE => false
                   | SOME x => (value' := SOME (f x); true),
                dependencies = fn () => [node]}
         in
            Event {value = value', node = node'}
         end

      fun mapPartial _ Never = Never
        | mapPartial p (Event {value, node}) =
         let
            val value' = ref NONE
            val node' = Node.new
               {compute = fn () =>
                  case !value of
                     NONE => false
                   | SOME x =>
                        case p x of
                           NONE => (value' := NONE; false)
                         | SOME y => (value' := SOME y; true),
                dependencies = fn () => [node]}
         in
            Event {value = value', node = node'}
         end

      fun filter _ Never = Never
        | filter p (Event {value, node}) =
         let
            val value' = ref NONE
            val node' = Node.new
               {compute = fn () =>
                  case !value of
                     NONE => false
                   | SOME x => if p x then (value' := SOME x; true) else false,
                dependencies = fn () => [node]}
         in
            Event {value = value', node = node'}
         end

      fun leftmost events =
         let
            fun getData Never = NONE
              | getData (Event x) = SOME x
            val eventData = List.mapPartial getData events
            val deps = List.map #node eventData

            val value' = ref NONE
            val node' = Node.new
               {compute = fn () =>
                   let
                      fun go [] = false
                        | go ({value, node} :: rest) =
                         if Node.isChanging node
                            then (value' := SOME (Option.valOf (!value)); true)
                         else go rest
                   in
                      go eventData
                   end,
                dependencies = fn () => deps}
         in
            Event {value = value', node = node'}
         end

      fun subscribe Never _ = ()
        | subscribe (Event {value, node}) k =
         let
            val sub = Node.subscribe node (fn () => k (Option.valOf (!value)))
         in
            DynArray.push (onCleanup, sub)
         end

      fun new () =
         let
            val value = ref NONE
            val node = Node.new
               {compute = fn () => Option.isSome (!value),
                dependencies = fn () => []}
            fun fire x =
               (value := SOME x
                ; Node.dirty node
                ; Node.stabilize ())
         in
            (Event {value = value, node = node}, fire)
         end

   end

structure Dynamic =
   struct
      datatype 'a value = Value of 'a | Uninit
      datatype 'a t = T of {value: 'a value ref, node: Node.t}

      fun r ::= x = r := Value x
      fun !! r =
         case !r of
            Value x => x
          | Uninit => raise Fail "Dynamic.!: uninitialized value"

      fun pure x =
         T {value = ref (Value x),
            node = Node.new {compute = fn () => true,
                             dependencies = fn () => []}}

      fun map f (T {value = v, node = n}) =
         let
            val value = ref Uninit
            val node = Node.new
               {compute = fn () => (value ::= f (!!v); true),
                dependencies = fn () => [n]}
         in
            T {value = value, node = node}
         end

      fun map2 f (T {value = v1, node = n1}, T {value = v2, node = n2}) =
         let
            val value = ref Uninit
            val node = Node.new
               {compute = fn () =>
                  (value ::= f (!!v1, !!v2); true),
                dependencies = fn () => [n1, n2]}
         in
            T {value = value, node = node}
         end

      fun ap (T {value = v1, node = n1}, T {value = v2, node = n2}) =
         let
            val value = ref Uninit
            val node = Node.new
               {compute = fn () =>
                  (value ::= !!v1 (!!v2); true),
                dependencies = fn () => [n1, n2]}
         in
            T {value = value, node = node}
         end

      fun (T {value = v, node = n}) >>= k =
         let
            val mainValueRef = ref Uninit
            val mainNodeRef: Node.t value ref = ref Uninit
            val rhsNodeRef: Node.t value ref = ref Uninit
            val rhsValueRef = ref Uninit
            val rhsNode = Node.new
               {compute = fn () =>
                  let
                     val rhs as T {node = rhsNode, value = rhsValue} = k (!!v)
                     val main = !!mainNodeRef
                     val () = Node.addDependent (rhsNode, main)
                  in
                     case !rhsNodeRef of
                        Uninit => ()
                      | Value oldRhs => Node.removeDependent (oldRhs, main)
                     ; rhsValueRef ::= !!rhsValue
                     ; rhsNodeRef ::= rhsNode
                     ; true
                  end,
                dependencies = fn () => [n]}
            val mainNode = Node.new
               {compute = fn () => (mainValueRef ::= !!rhsValueRef; true),
                dependencies = fn () =>
                  case !rhsNodeRef of
                     Uninit => [rhsNode]
                   | Value node => [node, rhsNode]}
         in
            mainNodeRef ::= mainNode
            ; T {node = mainNode, value = mainValueRef}
         end

      fun switch (T {value = v, node = n}) =
         let
         in
            raise Fail "NYI"
         end
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
