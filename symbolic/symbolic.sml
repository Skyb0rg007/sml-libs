


structure Symbolic =
struct

infix && || == ==>

structure Bool =
   struct
      type t = bool

      val not = not
      fun or (a, b) = a orelse b
      fun and_ (a, b) = a andalso b
      fun ite (a, b, c) = if a then b else c
      fun xor (a, b) = (a andalso not b) orelse (not a andalso b)
      fun implies (a, b) = not a orelse b
   end

structure SBool =
   struct
      datatype t =
         Const of bool
       | Sym of string * int
       | Or of t * t
       | And of t * t
       | Not of t
       | Equal of t * t
       | ITE of t * t * t

      val counter = ref 0

      fun sym s = Sym (s, !counter) before counter := !counter + 1

      fun not (Const b) = Const (Bool.not b)
        | not (Not b) = b
        | not b = Not b

      fun or (Const true, _) = Const true
        | or (Const false, b) = b
        | or (_, Const true) = Const true
        | or (a, Const false) = a
        | or (a, b) = Or (a, b)

      fun and_ (Const false, _) = Const false
        | and_ (Const true, b) = b
        | and_ (_, Const false) = Const false
        | and_ (a, Const true) = a
        | and_ (a, b) = And (a, b)

      fun ite (Const true, b, _) = b
        | ite (Const false, _, c) = c
        | ite (a, b, c) = ITE (a, b, c)

      fun xor (a, b) = or (and_ (a, not b), and_ (not a, b))

      fun implies (a, b) = or (not a, b)

      fun symeq (Const a, Const b) = Const (a = b)
        | symeq (Const true, b) = b
        | symeq (Const false, b) = not b
        | symeq (a, Const true) = a
        | symeq (a, Const false) = not a
        | symeq (a, b) = if a = b then Const true else Equal (a, b)

      fun a <= b = or (not a, b)

      fun a < b = and_ (not a, b)

      fun a >= b = or (a, not b)

      fun a > b = and_ (a, not b)
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
