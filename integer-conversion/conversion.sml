
structure Conversion =
struct

fun algorithm (arr : IntInf.int array, len, base) =
   let
      fun loop (i, j) =
         if i < len
            then
               let
                  val a = Array.sub (arr, i)
                  val b = Array.sub (arr, i + 1)
               in
                  Array.update (arr, i, 0);
                  Array.update (arr, i + 1, 0);
                  Array.update (arr, j, a * base + b);
                  loop (i + 2, j + 1)
               end
         else algorithm (arr, (len + 1) div 2, base * base)

      fun finish (acc, i) =
         if i < len
            then finish (acc * base + Array.sub (arr, i), i + 1)
         else acc
   in
      if len <= 40
         then finish (0, 0)
      else if len mod 2 = 0
         then loop (0, 0)
      else loop (1, 1)
   end

fun fromString str =
   let
      val len = String.size str
      val arrLen = (len + 1) div 2
      val arr = Array.array (arrLen, 0)
      fun index i = IntInf.fromInt (Char.ord (String.sub (str, i)) - 48)
      fun loop (i, j) =
         if i < len
            then (Array.update (arr, j, index i * 10 + index (i + 1));
                  loop (i + 2, j + 1))
         else algorithm (arr, arrLen, 100)
   in
      if len mod 2 = 0
         then loop (0, 0)
      else (Array.update (arr, 0, index 0); loop (1, 1))
   end

(* fun fromString str = *)
(*    let *)
(*       fun combine (b, d1 :: d2 :: ds) = d1 * b + d2 :: combine (b, ds) *)
(*         | combine (_, []) = [] *)
(*         | combine (_, [_]) = raise Fail "combine: odd length" *)

(*       fun loop (_, _, []) = 0 *)
(*         | loop (_, _, [d]) = d *)
(*         | loop (b, l, ds) = *)
(*          if l > 40 *)
(*             then *)
(*                let *)
(*                   val b' = b * b *)
(*                   val l' = (l + 1) div 2 *)
(*                   val ds' = if l mod 2 = 0 then ds else 0 :: ds *)
(*                in *)
(*                   loop (b', l', combine (b', ds')) *)
(*                end *)
(*          else List.foldl (fn (d, acc) => acc * b + d) 0 ds *)

(*       val digits = List.map (fn c => IntInf.fromInt (Char.ord c - 48)) (String.explode str) *)
(*    in *)
(*       loop (10, String.size str, digits) *)
(*    end *)

end

(* vim: set ts=3 sw=3: *)
