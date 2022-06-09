
signature VOID =
sig
   type t

   val absurd: t -> 'a
end

structure Void :> VOID =
struct
   type t = unit

   fun absurd () = raise Fail "Void.absurd"
end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
