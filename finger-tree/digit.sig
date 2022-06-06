
signature DIGIT =
sig

datatype 'a t =
   One of 'a
 | Two of 'a * 'a
 | Three of 'a * 'a * 'a
 | Four of 'a * 'a * 'a * 'a

(* Prepend a value. Raises `Fail` if digit is `Four` *)
val cons: 'a * 'a t -> 'a t

(* Append a value. Raises `Fail` if digit is `Four` *)
val snoc: 'a t * 'a -> 'a t

(* Access the first value *)
val head: 'a t -> 'a

(* Access the last value *)
val last: 'a t -> 'a

(* Remove the first value. Raises `Fail` if digit is `One` *)
val tail: 'a t -> 'a t

(* Remove the last value. Raises `Fail` if digit is `One` *)
val init: 'a t -> 'a t

(* Reverse the order *)
val reverse: 'a t -> 'a t

(* Map over a digit *)
val map: ('a -> 'b) -> 'a t -> 'b t

(* Map over a digit with position *)
val mapWithPos:
   ('pos * 'a -> 'pos) (* "Adding offset" *)
   -> ('pos * 'a -> 'b) (* mapping function with position argument *)
   -> 'pos * 'a t (* start position and digit *)
   -> 'b t

(* Map over a digit with contexts *)
val mapWithContext:
   ('ctx1 * 'a -> 'ctx1) (* left-to-right context *)
   -> ('a * 'ctx2 -> 'ctx2) (* right-to-left state *)
   -> ('ctx1 * 'a * 'ctx2 -> 'b) (* mapping function with context arguments *)
   -> 'ctx1 * 'a t * 'ctx2
   -> 'b t

(* Fold over a digit left-to-right with position *)
val foldlWithPos:
   ('pos * 'a -> 'pos)
   -> ('pos * 'a * 'b -> 'b)
   -> 'b
   -> 'pos * 'a t
   -> 'b

(* Fold over a digit right-to-left with position *)
val foldrWithPos:
   ('pos * 'a -> 'pos)
   -> ('pos * 'a * 'b -> 'b)
   -> 'b
   -> 'pos * 'a t
   -> 'b

(* Fold over a digit left-to-right with contexts *)
val foldlWithContext:
   ('ctx1 * 'a -> 'ctx1)
   -> ('a * 'ctx2 -> 'ctx2)
   -> ('ctx1 * 'a * 'ctx2 * 'b -> 'b)
   -> 'b
   -> 'ctx1 * 'a t * 'ctx2
   -> 'b

(* Fold over a digit right-to-left with contexts *)
val foldrWithContext:
   ('ctx1 * 'a -> 'ctx1)
   -> ('a * 'ctx2 -> 'ctx2)
   -> ('ctx1 * 'a * 'ctx2 * 'b -> 'b)
   -> 'b
   -> 'ctx1 * 'a t * 'ctx2
   -> 'b

(* Fold over a digit by mapping and combining the results *)
val foldMap: ('b * 'b -> 'b) -> ('a -> 'b) -> 'a t -> 'b

(* Standard left-to-right fold *)
val foldl: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

(* Standard right-to-left fold *)
val foldr: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

(* Reduce the values in the digit with a combining function *)
val reduce: ('a * 'a -> 'a) -> 'a t -> 'a

(* Given a monotonic position predicate, split the digit *)
val split:
   ('pos * 'a -> 'pos)
   -> ('pos -> bool)
   -> 'pos * 'a t
   -> 'a t option * 'a * 'a t option

(* Given a monotonic context predicate, split the digit *)
val search:
   ('ctx1 * 'a -> 'ctx1) (* forwards context *)
   -> ('a * 'ctx2 -> 'ctx2) (* backwards context *)
   -> ('ctx1 * 'ctx2 -> bool) (* predicate *)
   -> 'ctx1 * 'a t * 'ctx2 (* start context, digit, end context *)
   -> 'a t option * 'a * 'a t option (* prefix, match, suffix *)

val toString: ('a -> string) -> 'a t -> string

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
