(* Docstrings taken from http://mlton.org/MLtonVector *)

signature VECTOR_EXTRA =
sig

(* `create n`
 * Initiates the construction of a vector of length `n`,
 *   returning functions to manipulate the vector.
 * `done` may be called to return the created vector
 *   It is an error to call `done` before all entries are initialized
 *   It is an error to call `done` after calling `done`
 * `sub` may be called to return an initialized vector entry
 *   It is ok to call `sub` after calling `done`
 * `update` may be called to initialize a vector entry
 *   It is an error to call `update` after calling `done`
 * One must initialize entries in order from lowest to highest
 *
 * Most of these conditions are checked and may result in a raised exception
 * [MLton] Each function is constant-time
 * [SML/NJ] `sub` and `update` are constant-time; `done` is linear in `n`
 *)
val create: int -> {done: unit -> 'a vector, sub: int -> 'a, update: int * 'a -> unit}

(* `unfoldli (n, b, f)`
 * Constructs a vector υ of length `n`, whose elements υᵢ are determined by:
 *   b₀ = b
 *   (vᵢ, bᵢ₊₁) = f (i, bᵢ)
 *
 * This function is linear in `n`, though much faster for MLton than SML/NJ
 *)
val unfoldli: int * 'b * (int * 'b -> 'a * 'b) -> 'a vector * 'b

end

(* vim: set tw=0 ts=3 sw=3: *)
