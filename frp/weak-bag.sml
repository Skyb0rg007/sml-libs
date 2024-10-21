
structure WeakBag: WEAK_BAG =
struct

structure Weak = SMLofNJ.Weak

datatype 'a t = Bag of {
      nextId: int ref,
      children: 'a Weak.weak IntRedBlackMap.map ref
   }

datatype 'a ticket = Ticket of {weak: 'a Weak.weak, item: 'a, cleanup: unit -> unit}

fun insert (Bag {nextId, children}, x) =
   let
      val id = !nextId
      val () = nextId := id + 1
      fun cleanup () =
         case IntRedBlackMap.findAndRemove (!children, id) of
            NONE => ()
          | SOME (m, _) => children := m
      val wa = Weak.weak x
   in
      children := IntRedBlackMap.insert (!children, id, wa);
      Ticket {weak = wa, item = x, cleanup = cleanup}
   end

fun new () = Bag {nextId = ref 0, children = ref IntRedBlackMap.empty}

fun children (Bag {children, ...}) =
   (children := IntRedBlackMap.filter (Option.isSome o Weak.strong) (!children);
    !children)

fun isEmpty b = IntRedBlackMap.isEmpty (children b)

fun app f (Bag {children, ...}) =
   children := IntRedBlackMap.filter
      (fn w =>
         case Weak.strong w of
            NONE => false
          | SOME a => (f a; true))
      (!children)

fun remove (Ticket {cleanup, ...}) = cleanup ()

end

(* vim: set tw=0 ts=3 sw=3: *)
