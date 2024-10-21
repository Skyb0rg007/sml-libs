
signature FOLD =
sig

type ('a, 'b) t

(* Simple folds that cannot return early *)
val foldl : ('a * 'b -> 'b) -> 'b -> ('a, 'b) t
val foldl1 : ('a * 'a -> 'a) -> ('a, 'a option) t
val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) t

(* Modify the result of a fold *)
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

(* A stream that immediately returns a given result *)
val pure : 'b -> ('a, 'b) t

(* Sequential composition
 * The second fold is run once the first is finished
 * Note: The resulting fold does not support scanning
 * Note: Applying this function n times is a O(n^2) cost *)
val splitWith : ('a * 'b -> 'c) -> ('x, 'a) t * ('x, 'b) t -> ('x, 'c) t

(* Parallel composition
 * Both folds are fed all inputs, and the result terminates once both inputs do *)
val teeWith : ('a * 'b -> 'c) -> ('x, 'a) t * ('x, 'b) t -> ('x, 'c) t

(* Like teeWith, but terminates when the first fold terminates *)
val teeWithFst : ('a * 'b -> 'c) -> ('x, 'a) t * ('x, 'b) t -> ('x, 'c) t

(* Like teeWith, but terminates when one of the folds terminates *)
val teeWithMin : ('a * 'b -> 'c) -> ('x, 'a) t * ('x, 'b) t -> ('x, 'c) t

(* Dynamically select the running fold
 * The inner fold runs until it is completed
 * Note: The resulting fold does not support scanning *)
val concatMap : ('b -> ('a, 'c) t) -> ('a, 'b) t -> ('a, 'c) t

val runList : ('a, 'b) t * 'a list -> 'b

val driveWith : ('s -> ('a * 's) option) -> 's -> ('a, 'b) t -> 'b

val drive : ('a, 'b) Unfold.t -> ('b, 'c) t -> 'a -> 'c

end
