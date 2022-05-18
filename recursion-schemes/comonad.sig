(* Comonads
 * This signature is minimal needed for recursion schemes *)

signature COMONAD =
sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val extract: 'a t -> 'a
  val duplicate: 'a t -> 'a t t
end
