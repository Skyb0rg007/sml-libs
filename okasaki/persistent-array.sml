
signature PERSISTENT_ARRAY =
   sig
      type 'a array

      val array : int * 'a -> 'a array
      val fromList : 'a list -> 'a array
      val tabulate : int * (int -> 'a) -> 'a array
      val length : 'a array -> int
      (* val sub : 'a array * int -> 'a *)
      (* val update : 'a array * int * 'a -> 'a array *)
      (* val appi : (int * 'a -> unit) -> 'a array -> unit *)
      (* val app : ('a -> unit) -> 'a array -> unit *)
      (* val mapi : (int * 'a -> 'b) -> 'a array -> 'b array *)
      (* val map : ('a -> 'b) -> 'a array -> 'b array *)
   end

structure PersistentArray : PERSISTENT_ARRAY =
   struct
      datatype 'a node = Array of 'a Array.array
                       | Delta of int * 'a * 'a array

      withtype 'a array = 'a node ref

      fun array (n, x) = ref (Array (Array.array (n, x)))

      fun fromList xs = ref (Array (Array.fromList xs))

      fun tabulate (n, f) = ref (Array (Array.tabulate (n, f)))

      fun reroot (ref (Array arr)) = arr
        | reroot t =
         let
            fun go (t, k) =
               case !t of
                  Array arr => k arr
                | Delta (i, x, t') =>
                     go (t', fn arr =>
                        let
                           val y = Array.sub (arr, i)
                        in
                           Array.update (arr, i, x);
                           t := Array arr;
                           t' := Delta (i, y, t);
                           k arr
                        end)
         in
            go (t, fn arr => arr)
         end

      fun reroot' (ref (Array arr)) = arr
        | reroot' t =
         let
            datatype 'a cont = Empty
                             | Update of int * 'a * 'a array * 'a cont

            fun apply (Empty, arr) = arr
              | apply (Update (i, x, t', k), arr) =
               let
                  val y = Array.sub (arr, i)
               in
                  Array.update (arr, i, x);
                  t := Array arr;
                  t' := Delta (i, y, t);
                  apply (k, arr)
               end

            fun go (t, k) =
               case !t of
                  Array arr => apply (k, arr)
                | Delta (i, x, t') => go (t', Update (i, x, t', k))
         in
            go (t, Empty)
         end

      fun length arr = Array.length (reroot arr)

      fun app f arr = Array.app f (reroot arr)
      fun appi f arr = Array.appi f (reroot arr)
   end

(* vim: set ts=3 sw=3: *)
