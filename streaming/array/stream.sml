
structure Stream =
struct

datatype 'a push = Push of ('a -> unit) -> unit

datatype 'a pull = Pull of (int -> 'a) * int

structure Push =
   struct
      datatype t = datatype push

      fun map f (Push k) = Push (fn g => k (g o f))

      fun append (Push k1, Push k2) = Push (fn f => (k1 f; k2 f))

      val empty = Push (fn _ => ())

      fun singleton x = Push (fn f => f x)

      fun cons (x, Push k) = Push (fn f => (f x; k f))

      fun snoc (Push k, x) = Push (fn f => (k f; f x))

      fun unzip (Push k) =
         let
            val a1 = ref empty
            val a2 = ref empty
         in
            k (fn (x, y) =>
               (a1 := snoc (!a1, x);
                a2 := snoc (!a2, y)));
            (!a1, !a2)
         end

      fun toList (Push k) =
         let
            val lst = ref []
         in
            k (fn x => lst := x :: !lst);
            List.rev (!lst)
         end
   end

structure Pull =
   struct
   end


end

(* vim: set tw=0 ts=3 sw=3: *)
