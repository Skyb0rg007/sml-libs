
structure Free =
struct
   datatype ('r, 'a) state =
      Put of 'r * (unit -> 'a)
    | Get of 'r -> 'a

   type 'a f = (int, 'a) state

   fun stateMap f (Put (n, k)) = Put (n, f o k)
     | stateMap f (Get k) = Get (f o k)

   datatype 'a free =
      Pure of 'a
    | Impure of 'a free f

   val get = Impure (Get Pure)
   fun put x = Impure (Put (x, Pure))
   val pure = Pure
   fun map f (Pure x) = Pure (f x)
     | map f (Impure m) = Impure (stateMap (map f) m)
   fun bind (Pure x) f = f x
     | bind (Impure m) f = Impure (stateMap (fn x => bind x f) m)

   fun run t =
      let
         val i = ref 0

         fun go (Pure x) = x
           | go (Impure (Get k)) = go (k (!i))
           | go (Impure (Put (n, k))) = (i := n; go (k ()))
      in
         go t
      end

   fun prog () =
      bind get (fn n =>
      if n < 0
         then pure n
      else
         bind (put (n - 1)) prog)
end

(* vim: set tw=0 ts=3 sw=3: *)
