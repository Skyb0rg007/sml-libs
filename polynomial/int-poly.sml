
structure IntPoly: POLY =
struct

structure V = Vector

structure Coeff =
   struct
      type t = int

      val op + = Int.+
      val op - = Int.-
      val op * = Int.*
      val ~ = Int.~
      val zero = 0
      val one = 1
      val toString = Int.toString

      fun equals (a: t, b) = a = b
   end

datatype t = T of int vector

fun toString (T xs) =
   if V.length xs = 0
      then "0"
   else if V.length xs = 1
      then Coeff.toString (V.sub (xs, 0))
   else
      let
         fun go (0, x, acc) = Coeff.toString x ^ acc
           | go (1, x, acc) = Coeff.toString x ^ " * X + " ^ acc
           | go (i, x, acc) = Coeff.toString x ^ " * X^" ^ Int.toString i ^ " + " ^ acc
      in
         Vector.foldli go "" xs
      end

fun constant a =
   if Coeff.equals (a, Coeff.zero)
      then T #[]
   else T #[a]

val trivialCoeff = Coeff.equals (Coeff.zero, Coeff.one)

val var =
   if trivialCoeff
      then T #[]
   else T #[Coeff.zero, Coeff.one]

val isVar =
   if trivialCoeff
      then fn T xs => V.length xs = 0
   else fn T xs =>
      V.length xs = 2
      andalso V.sub (xs, 0) = Coeff.zero
      andalso V.sub (xs, 1) = Coeff.one

fun add (T xs, T ys) =
   let
      val lenXs = V.length xs
      val lenYs = V.length ys
      val lenMin = Int.min (lenXs, lenYs)
      val lenMax = Int.max (lenXs, lenYs)
      fun gen i =
         if i < lenMin
            then Coeff.+ (V.sub (xs, i), V.sub (ys, i))
         else if lenXs < lenYs
            then V.sub (ys, i)
         else V.sub (xs, i)
   in
      T (V.tabulate (lenMax, gen))
   end

fun sub (T xs, T ys) =
   let
      val lenXs = V.length xs
      val lenYs = V.length ys
      val lenMin = Int.min (lenXs, lenYs)
      val lenMax = Int.max (lenXs, lenYs)
      fun gen i =
         if i < lenMin
            then Coeff.- (V.sub (xs, i), V.sub (ys, i))
         else if lenXs < lenYs
            then Coeff.~ (V.sub (ys, i))
         else V.sub (xs, i)
   in
      T (V.tabulate (lenMax, gen))
   end

fun neg (T xs) = T (V.map Coeff.~ xs)

fun convolution (T xs, T ys) =
   let
      val lenXs = V.length xs
      val lenYs = V.length ys
      val lenZs = lenXs + lenYs - 1
      fun gen k =
         let
            val start = Int.max (k - lenYs + 1, 0)
            val stop = Int.min (k, lenXs - 1)
            fun go (i, acc) =
               if i > stop
                  then acc
               else go (i + 1, Coeff.+ (acc, Coeff.* (V.sub (xs, i), V.sub (ys, k - i))))
         in
            go (start, 0)
         end
   in
      if lenXs = 0 andalso lenYs = 0
         then T #[]
      else T (V.tabulate (lenZs, gen))
   end

fun evaluate (T cs, x) =
   let
      fun go (cn, (acc, xn)) =
         (Coeff.+ (acc, Coeff.* (cn, xn)), Coeff.* (x, xn))
   in
      #1 (V.foldl go (0, 1) cs)
   end

fun degree (T xs) =
   if V.length xs = 0
      then 0
   else V.length xs - 1

(* fun derivative (T xs) = *)
(*    if V.length xs = 0 *)
(*       then T #[] *)
(*    else V.tabulate (V.length xs - 1, fn i => ) *)

val op + = add
val op - = sub
val ~ = neg
val op * = convolution

end

(* vim: set tw=0 ts=3 sw=3: *)
