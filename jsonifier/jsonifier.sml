
structure Jsonifier:
sig

type t

val null: t
val true_: t
val false_: t
val bool: bool -> t
val int: int -> t
(* Precondition: must be valid utf8 *)
val string: string -> t
val array: t list -> t
val object: (string * t) list -> t

(* Include raw string in the output *)
val rawString: string -> t

(* toArray is cheapest, toString requires a copy *)
val toArray: t -> CharArray.array
val toString: t -> string

end =
struct

structure A = CharArray
structure V = CharVector
structure W = Word

datatype t = T of {size: int, poke: int * A.array -> int}

fun toArray (T {size, poke}) =
   let
      val arr = A.array (size, #"\000")
      val len = poke (0, arr)
   in
      if size <> len
         then
            raise Fail (String.concat
               ["Internal error: mismatch between size (",
                Int.toString size,
                ") and poke (",
                Int.toString len,
                ")"])
      else ();
      arr
   end

fun toString x = A.vector (toArray x)

fun rawString s =
   let
      val n = String.size s
   in
      T {
         size = n,
         poke = fn (i, a) => (A.copyVec {src = s, dst = a, di = i}; i + n)
      }
   end

val null = rawString "null"
val true_ = rawString "true"
val false_ = rawString "false"

fun bool true = true_
  | bool false = false_

datatype char_size = One | Two of char | Six

fun charSize #"\b" = Two #"b"
  | charSize #"\f" = Two #"f"
  | charSize #"\n" = Two #"n"
  | charSize #"\r" = Two #"r"
  | charSize #"\t" = Two #"t"
  | charSize #"\"" = Two #"\""
  | charSize #"\\" = Two #"\\"
  | charSize c = if c < #" " then Six else One

fun encodeLength s =
   let
      val len = String.size s

      fun go (i, acc) =
         if i >= len
            then acc
         else
            let
               val c = String.sub (s, i)
               val n = Char.ord c
            in
               if n < 0x80
                  then
                     case charSize c of
                        One => go (i + 1, acc + 1)
                      | Two _ => go (i + 1, acc + 2)
                      | Six => go (i + 1, acc + 6)
               else if n < 0xe0
                  then go (i + 2, acc + 2)
               else if n < 0xf0
                  then go (i + 3, acc + 3)
               else go (i + 4, acc + 4)
            end
   in
      go (0, 0)
   end

fun encodeStr (s, i, a) =
   let
      val len = String.size s

      fun go (i, j) =
         if j >= len
            then (A.update (a, i, #"\""); i + 1)
         else
            let
               val c = String.sub (s, j)
               val n = Char.ord c
            in
               if n < 0x80
                  then
                     case charSize c of
                        One =>
                           (A.update (a, i, c);
                            go (i + 1, j + 1))
                      | Two c' =>
                           (A.update (a, i, #"\\");
                            A.update (a, i + 1, c');
                            go (i + 2, j + 1))
                      | Six =>
                           let
                              val w = W.fromInt (Char.ord c)
                              val w1 = W.andb (W.>> (w, 0w12), 0wxf)
                              val w2 = W.andb (W.>> (w, 0w8), 0wxf)
                              val w3 = W.andb (W.>> (w, 0w4), 0wxf)
                              val w4 = W.andb (w, 0wxf)
                              val digits = "0123456789abcdef"
                           in
                              A.update (a, i, #"\\");
                              A.update (a, i + 1, #"u");
                              A.update (a, i + 2, V.sub (digits, W.toInt w1));
                              A.update (a, i + 3, V.sub (digits, W.toInt w2));
                              A.update (a, i + 4, V.sub (digits, W.toInt w3));
                              A.update (a, i + 5, V.sub (digits, W.toInt w4));
                              go (i + 6, j + 1)
                           end
               else if n < 0xe0
                  then (A.update (a, i, c);
                        A.update (a, i + 1, String.sub (s, j + 1));
                        go (i + 2, j + 2))
               else if n < 0xf0
                  then (A.update (a, i, c);
                        A.update (a, i + 1, String.sub (s, j + 1));
                        A.update (a, i + 2, String.sub (s, j + 2));
                        go (i + 3, j + 3))
               else 
                  (A.update (a, i, c);
                   A.update (a, i + 1, String.sub (s, j + 1));
                   A.update (a, i + 2, String.sub (s, j + 2));
                   A.update (a, i + 3, String.sub (s, j + 3));
                   go (i + 4, j + 4))
            end
   in
      A.update (a, i, #"\"");
      go (i + 1, 0)
   end

fun string s = T {
      size = 2 + encodeLength s,
      poke = fn (i, a) => encodeStr (s, i, a)
   }

fun int x = T {
      size = 
         if x < 0
           then
             if x < ~9999999999
               then
                 if x < ~99999999999999
                   then
                     if x < ~9999999999999999
                       then
                         if x < ~99999999999999999
                           then
                             if x < ~999999999999999999
                               then 20
                               else 19
                           else 18
                       else
                         if x < ~999999999999999
                           then 17
                           else 16
                   else
                     if x < ~999999999999
                       then
                         if x < ~9999999999999
                           then 15
                           else 14
                       else
                         if x < ~99999999999
                           then 13
                           else 12
               else
                 if x < ~99999
                   then
                     if x < ~9999999
                       then
                         if x < ~99999999
                           then
                             if x < ~999999999
                               then 11
                               else 10
                           else 9
                       else
                         if x < ~999999
                           then 8
                           else 7
                   else
                     if x < ~99
                       then
                         if x < ~999
                           then
                             if x < ~9999
                               then 6
                               else 5
                           else 4
                       else
                         if x < ~9
                           then 3
                           else 2
           else
             if x > 9999999999
               then
                 if x > 99999999999999
                   then
                     if x > 9999999999999999
                       then
                         if x > 99999999999999999
                           then
                             if x > 999999999999999999
                               then 19
                               else 18
                           else 17
                       else
                         if x > 999999999999999
                           then 16
                           else 15
                   else
                     if x > 999999999999
                       then
                         if x > 9999999999999
                           then 14
                           else 13
                       else
                         if x > 99999999999
                           then 12
                           else 11
               else
                 if x > 99999
                   then
                     if x > 9999999
                       then
                         if x > 99999999
                           then
                             if x > 999999999
                               then 10
                               else 9
                           else 8
                       else
                         if x > 999999
                           then 7
                           else 6
                   else
                     if x > 99
                       then
                         if x > 999
                           then
                             if x > 9999
                               then 5
                               else 4
                           else 3
                       else
                         if x > 9
                           then 2
                           else 1,
      poke = fn (i, a) =>
         let
            val (i0, x0) =
               if x < 0
                  then (A.update (a, i, #"-"); (i + 1, ~x))
               else (i, x)
            val digits = "0123456789"

            fun go (i, x) =
               if x = 0
                  then i
               else
                  let
                     val x' = x div 10
                  in
                     A.update (a, i, V.sub (digits, x - x' * 10));
                     go (i + 1, x')
                  end
            val i' = go (i0, x0)

            fun reverse (i, j) =
               if i >= j
                  then ()
               else
                  let
                     val c = A.sub (a, i)
                  in
                     A.update (a, i, A.sub (a, j));
                     A.update (a, j, c);
                     reverse (i + 1, j - 1)
                  end
            val () = reverse (i0, i' - 1)
         in
            i'
         end
   }

fun commas n =
   if n <= 1
      then 0
   else n - 1

fun array xs = T {
      size =
         let
            fun f (T {size, ...}, {nelems, content}) =
               {nelems = nelems + 1, content = content + size}

            val init = {nelems = 0, content = 0}
            val {nelems, content} = List.foldl f init xs
         in
            2 + commas nelems + content
         end,
      poke = fn (i, a) =>
         let
            fun go (T {size, poke}, (true, i)) =
               (false, poke (i, a))
              | go (T {size, poke}, (false, i)) =
              (A.update (a, i, #",");
               (false, poke (i + 1, a)))

            val () = A.update (a, i, #"[");
            val (_, i') = List.foldl go (true, i + 1) xs
         in
            A.update (a, i', #"]");
            i' + 1
         end
   }

fun object xs =
   let
      fun f ((key, T {size=n, poke=p}), (count, size, poke)) =
         let
            val first = count = 0
            val rowSize = encodeLength key + n
            fun rowPoke (i, a) =
               let
                  val i = encodeStr (key, i, a)
                  val () = A.update (a, i, #":")
                  val i = i + 1
               in
                  p (i, a)
               end
         in
            if first
               then (1, size + rowSize, rowPoke)
            else
               (count + 1,
                size + rowSize,
                fn (i, a) =>
                   let
                      val i = poke (i, a)
                      val () = A.update (a, i, #",")
                      val i = i + 1
                   in
                      rowPoke (i, a)
                   end)
         end

      val (count, size, poke) = List.foldl f (0, 0, fn (i, _) => i) xs
   in
      T {size = 2 + commas count + count * 3 + size,
         poke = fn (i, a) =>
            let
               val () = A.update (a, i, #"{")
               val i = i + 1
               val i = poke (i, a)
            in
               A.update (a, i, #"}");
               i + 1
            end}
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
