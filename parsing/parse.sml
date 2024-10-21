
structure Parse: PARSE =
struct

infix 1 >>=
infix 4 <$> <$ $> <*> <* *>
infixr 6 <|>

exception Fail' = Fail

datatype ('s, 'e, 'a) res = Ok of 'a * int * 's | Fail | Err of 'e

type ('s, 'e, 'a) t = string * int * 's -> ('s, 'e, 'a) res

fun map f p (str, n, s) =
   case p (str, n, s) of
      Ok (a, n, s) => Ok (f a, n, s)
    | Fail => Fail
    | Err e => Err e

fun f <$> p = map f p

fun (a <$ p) (str, n, s) =
   case p (str, n, s) of
      Ok (_, n, s) => Ok (a, n, s)
    | Fail => Fail
    | Err e => Err e

fun p $> a = a <$ p

fun pure a (_, n, s) = Ok (a, n, s)

fun (ff <*> fa) (str, n, s) =
   case ff (str, n, s) of
      Fail => Fail
    | Err e => Err e
    | Ok (f, n, s) =>
         case fa (str, n, s) of
            Ok (a, n, s) => Ok (f a, n, s)
          | Fail => Fail
          | Err e => Err e

fun (fa <* fb) (str, n, s) =
   case fa (str, n, s) of
      Fail => Fail
    | Err e => Err e
    | Ok (a, n, s) =>
         case fb (str, n, s) of
            Fail => Fail
          | Err e => Err e
          | Ok (_, n, s) => Ok (a, n, s)

fun (fa *> fb) (str, n, s) =
   case fa (str, n, s) of
      Fail => Fail
    | Err e => Err e
    | Ok (_, n, s) => fb (str, n, s)

fun (fa >>= f) (str, n, s) =
   case fa (str, n, s) of
      Fail => Fail
    | Err e => Err e
    | Ok (a, n, s) => f a (str, n, s)

fun fix f =
   let
      val r = ref (fn _ => raise Fail' "Parse.fix")
      fun p args = !r args
   in
      r := f p
      ; p
   end

fun get (_, n, s) = Ok (s, n, s)

fun put s (_, n, _) = Ok ((), n, s)

fun modify f (_, n, s) = Ok ((), n, f s)

fun empty _ = Fail

fun err e _ = Err e

fun withError f h (str, n, s) =
   case f (str, n, s) of
      Err e => (h e (str, n, s))
    | x => x

fun lookahead p (str, n, s) =
   case p (str, n, s) of
      Ok (a, _, _) => Ok (a, n, s)
    | x => x

fun fails p (str, n, s) =
   case p (str, n, s) of
      Fail => Ok ((), n, s)
    | Err e => Err e
    | Ok (_, _, _) => Fail

fun try p (str, n, s) =
   case p (str, n, s) of
      Err _ => Fail
    | x => x

fun (p1 <|> p2) (str, n, s) =
   case p1 (str, n, s) of
      Fail => p2 (str, n, s)
    | x => x

fun branch p1 p2 p3 (str, n, s) =
   case p1 (str, n, s) of
      Ok (_, n, s) => p2 (str, n, s)
    | Fail => p3 (str, n, s)
    | Err e => Err e

fun many p =
   let
      fun go (str, n, s) =
         case p (str, n, s) of
            Fail => Ok ([], n, s)
          | Err e => Err e
          | Ok (x, n, s) =>
               case go (str, n, s) of
                  Ok (xs, n, s) => Ok (x :: xs, n, s)
                | x => x
   in
      go
   end

fun many_ p =
   let
      fun go (str, n, s) =
         case p (str, n, s) of
            Ok (_, n, s) => go (str, n, s)
          | Fail => Ok ((), n, s)
          | Err e => Err e
   in
      go
   end

fun some_ p = p *> many_ p

fun optional p = (SOME <$> p) <|> pure NONE

fun eof (str, n, s) =
   if String.size str = n
      then Ok ((), n, s)
   else Fail

fun take m =
   if m < 0
      then raise Fail' "Parse.take: negative integer"
   else fn (str, n, s) =>
      if m <= String.size str - n
         then Ok (Substring.substring (str, n, m), n + m, s)
      else Fail

fun anyChar (str, n, s) =
   if n < String.size str
      then Ok (String.sub (str, n), n + 1, s)
   else Fail

fun char c (str, n, s) =
   if n < String.size str andalso String.sub (str, n) = c
      then Ok ((), n + 1, s)
   else Fail

fun satisfy f (str, n, s) =
   if n < String.size str andalso f (String.sub (str, n))
      then Ok ((), n + 1, s)
   else Fail

fun string bytes (str, n, s) =
   if String.size bytes <= String.size str - n
      andalso Substring.isPrefix bytes (Substring.extract (str, n, NONE))
      then Ok ((), n + String.size bytes, s)
   else Fail

local
   open Word
   infix orb <<
   fun c2w c = Word.fromInt (Char.ord c)
in
   (* Construct a codepoint from the given number of utf8-encoded bytes *)
   fun wc1 c = c2w c

   fun wc2 (c1, c2) =
      ((c2w c1 - 0wxc0) << 0w6)
      orb (c2w c2 - 0wx80)

   fun wc3 (c1, c2, c3) =
      ((c2w c1 - 0wxe0) << 0w12)
      orb ((c2w c2 - 0wx80) << 0w6)
      orb (c2w c3 - 0wx80)

   fun wc4 (c1, c2, c3, c4) =
      ((c2w c1 - 0wxf0) << 0w18)
      orb ((c2w c2 - 0wx80) << 0w12)
      orb ((c2w c3 - 0wx80) << 0w6)
      orb (c2w c4 - 0wx80)
end

fun anyWChar (str, n, s) =
   if n = String.size str
      then Fail
   else
      let
         val c1 = String.sub (str, n)
      in
         if c1 <= #"\u007f"
            then Ok (wc1 c1, n + 1, s)
         else if n + 1 = String.size str
            then Fail
         else
            let
               val c2 = String.sub (str, n + 1)
            in
               if c2 <= #"\u00df"
                  then Ok (wc2 (c1, c2), n + 2, s)
               else if n + 2 = String.size str
                  then Fail
               else
                  let
                     val c3 = String.sub (str, n + 2)
                  in
                     if c3 <= #"\u00ef"
                        then Ok (wc3 (c1, c2, c3), n + 3, s)
                     else if n + 3 = String.size str
                        then Fail
                     else
                        let
                           val c4 = String.sub (str, n + 3)
                        in
                           Ok (wc4 (c1, c2, c3, c4), n + 4, s)
                        end
                  end
            end
      end

fun withSubstring p f (str, n, s) =
   case p (str, n, s) of
      Fail => Fail
    | Err e => Err e
    | Ok (a, n', s) => f (a, Substring.substring (str, n, n' - n)) (str, n', s)

structure R =
   struct
      datatype t = datatype res
   end

datatype ('s, 'e, 'a) result =
   Ok of 'a * 's * substring
 | Fail
 | Err of 'e

fun run f s str =
   case f (str, 0, s) of
      R.Fail => Fail
    | R.Err e => Err e
    | R.Ok (a, n, s) => Ok (a, s, Substring.extract (str, n, NONE))

end

(* vim: set tw=0 ts=3 sw=3: *)
