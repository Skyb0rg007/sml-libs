
structure EventHandler :>
sig
   type 'a handler = 'a -> unit

   type 'a add_handler

   val register: 'a add_handler -> 'a handler -> unit -> unit
   val newAddHandler: unit -> 'a add_handler * 'a handler
   val map: ('a -> 'b) -> 'a add_handler -> 'b add_handler
   val filter: ('a -> bool) -> 'a add_handler -> 'a add_handler
end =
struct
   type 'a handler = 'a -> unit

   datatype 'a add_handler =
      AddHandler of 'a handler -> unit -> unit

   fun map f (AddHandler g) = AddHandler (fn h => g (h o f))

   fun filter p (AddHandler g) =
      AddHandler (fn h => g (fn x => if p x then h x else ()))

   fun register (AddHandler f) = f

   structure T = IntHashTable

   val counter = ref 0
   fun fresh () = !counter before counter := !counter + 1

   fun newAddHandler () =
      let
         val handlers = T.mkTable (2, Fail "EventHandler.newAddHandler")

         fun register h =
            let
               val key = fresh ()
               fun remove () =
                  if T.inDomain handlers key
                     then ignore (T.remove handlers key)
                  else ()
            in
               T.insert handlers (key, h)
               ; remove
            end

         fun runHandlers x =
            T.app (fn f => f x) handlers
      in
         (AddHandler register, runHandlers)
      end
end

(* vim: set ft=sml sw=3 ts=3 tw=0: *)
