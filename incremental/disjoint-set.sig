
signature DISJOINT_SET =
sig
   type 'a t

   (* Standard `ref` operations *)
   val make: 'a -> 'a t
   val ! : 'a t -> 'a
   val := : 'a t * 'a -> unit

   (* Do both references refer to the same reference? *)
   val same: 'a t * 'a t -> bool

   (* Union the references *)
   val union: 'a t * 'a t -> unit

   (* Somewhat internal functions *)
   val representative: 'a t -> 'a t
   val isRepresentative: 'a t -> bool
   val shallowEq: 'a t * 'a t -> bool
end

(* vim: set tw=0 ts=3 sw=3: *)
