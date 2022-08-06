
structure WeakBag :>
sig
   type 'a t
   type 'a ticket

   val empty: unit -> 'a t
   val app: ('a -> unit) -> 'a t -> unit
end =
struct
   open SMLofNJ.Weak
   structure M = IntRedBlackMap

   datatype 'a t = WeakBag of {
      nextId: int ref,
      children: 'a weak M.map ref
   }

   datatype 'a ticket = Ticket of {
      weak: 'a weak,
      item: 'a
   }

   fun empty () = WeakBag {nextId = ref 1, children = ref M.empty}

   fun app f (WeakBag {children, ...}) =
      let
         fun f' w =
            case strong w of
               NONE => ()
             | SOME x => f x
      in
         M.app f' (!children)
      end

   (* fun insert (WeakBag {nextId, children}, ) *)
end

(* vim: set ft=sml sw=3 ts=3 tw=0: *)
