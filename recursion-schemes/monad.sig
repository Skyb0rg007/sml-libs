(* Monads
 * This signature is minimal needed for recursion schemes *)

signature MONAD =
sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val pure: 'a -> 'a t
  val join: 'a t t -> 'a t
end
