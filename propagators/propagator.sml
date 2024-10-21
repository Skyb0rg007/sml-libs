
structure Propagator: PROPAGATOR =
struct

datatype 'a change =
   Unchanged
 | Changed of 'a

structure Scheduler =
   struct
      val tasks: (unit -> unit) list ref = ref []

      fun enqueue task =
         tasks := task :: !tasks

      fun run () =
         case !tasks of
            [] => ()
          | task :: rest => (tasks := rest; task ())
   end

structure Cell =
   struct
      val blockWrites = ref false

      datatype 'a t = T of {
            merge: 'a * 'a -> 'a change,
            value: 'a ref,
            prop: ('a -> unit) ref
         }

      fun new value merge = T {
            merge = merge,
            value = ref value,
            prop = ref (fn _ => ())
         }

      fun content (T {value, ...}) = !value

      fun write (T {merge, value, prop}) newValue =
         if !blockWrites
            then raise Fail "Cannot call Cell.write in a Cell.watch callback!"
         else
            let
               val oldValue = !value
            in
               case merge (oldValue, newValue) of
                  Unchanged => ()
                | Changed mergedValue =>
                     (value := mergedValue
                      ; blockWrites := true
                      ; !prop mergedValue
                      ; blockWrites := false)
            end

      fun watch (T {value, prop, ...}) k =
         let
            val p = !prop
         in
            prop := (fn x => (p x; k x))
            ; blockWrites := true
            ; k (!value)
            ; blockWrites := false
         end
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
