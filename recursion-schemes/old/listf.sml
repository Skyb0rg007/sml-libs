
structure ListF =
struct
  structure F =
  struct
    datatype 'a t =
       NilF
     | ConsF of int * 'a

    fun map _ NilF = NilF
      | map f (ConsF (a, b)) = ConsF (a, f b)
  end

  structure Fix = FixFn(F)
  structure Free = FreeFn(F)
  structure Cofree = CofreeFn(F)
  structure Cata = CataFn(structure F = F structure Fix = Fix)
  structure Para = ParaFn(structure F = F structure Fix = Fix)

  fun toString xs =
    let
      fun go (Fix.Fix F.NilF) acc = List.rev acc
        | go (Fix.Fix (F.ConsF (n, rest))) acc =
            go rest (Int.toString n :: acc)
    in
      "[" ^ String.concatWith ", " (go xs []) ^ "]"
    end

  val Nil = Fix.Fix F.NilF
  fun Cons (x, xs) = Fix.Fix (F.ConsF (x, xs))
end

