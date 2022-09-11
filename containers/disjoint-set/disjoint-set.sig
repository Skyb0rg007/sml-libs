
signature DISJOINT_SET =
sig
   type 'a t

   (* Standard `ref` operations *)
   val make: 'a -> 'a t
   val ! : 'a t -> 'a
   val := : 'a t * 'a -> unit

   (* Do both arguments refer to the same set? *)
   val equals: 'a t * 'a t -> bool

   (* Union the references.
    * This arbitrarily chooses one of the values to keep, so ensure compatibility first *)
   val union: 'a t * 'a t -> unit
end

(* vim: set tw=0 ts=3 sw=3: *)
