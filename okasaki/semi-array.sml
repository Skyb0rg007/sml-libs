
signature SEMI_ARRAY =
   sig
      type 'a t

      val array : int * 'a -> 'a t
      val sub : 'a t * int -> 'a
      val update : 'a t * int * 'a -> 'a t
   end

structure SemiArray : SEMI_ARRAY =
   struct
      datatype 'a data =
         Newest of 'a array
       | Diff of int * 'a * 'a t

      withtype 'a t = 'a data ref

      fun array (n, v) = ref (Newest (Array.array (n, v)))

      fun reroot (ref (Newest arr)) = arr
        | reroot (t as ref (Diff (i, v, t'))) =
         let
            val arr = reroot t'
         in
            Array.update (arr, i, v);
            t := Newest arr;
            arr
         end

      fun sub (t, i) = Array.sub (reroot t, i)

      fun update (t, i, x) =
         let
            val arr = reroot t
            val old = Array.sub (arr, i)
            val () = Array.update (arr, i, x)
            val res = ref (!t)
         in
            t := Diff (i, old, res);
            res
         end
   end

(* vim: set ts=3 sw=3: *)
