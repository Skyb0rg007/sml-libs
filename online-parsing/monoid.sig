
signature MONOID =
sig
   type t

   val zero: t
   val + : t * t -> t
end

(* vim: set tw=0 ts=3 sw=3: *)
