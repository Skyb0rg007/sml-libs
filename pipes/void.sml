
structure Void :>
sig
   type t

   val absurd: t -> 'a
end =
struct
   type t = unit

   exception Void

   fun absurd () = raise Void
end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
