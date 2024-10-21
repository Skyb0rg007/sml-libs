
structure FromJSON:
sig

type 'a t

type value = JSON.value
type object = (string * value) list
type array = value list
type 'a parser = value -> 'a t

val run: 'a t -> (string, 'a) Either.either 
val runDbg: ('a -> string) -> 'a t -> unit
val pure: 'a -> 'a t
val fail: string -> 'a t
val map: ('a -> 'b) -> 'a t -> 'b t
val or: 'a t * 'a t -> 'a t
val >>= : 'a t * ('a -> 'b t) -> 'b t

val withObject: string -> (object -> 'a t) -> 'a parser
val withString: string -> (string -> 'a t) -> 'a parser
val withArray: string -> (array -> 'a t) -> 'a parser
val withBool: string -> (bool -> 'a t) -> 'a parser
val withInt: string -> (IntInf.int -> 'a t) -> 'a parser
val withFloat: string -> (real -> 'a t) -> 'a parser
val parseField: 'a parser -> object -> string -> 'a t
val parseIndex: 'a parser -> array -> int -> 'a t

val bool: bool parser
val string: string parser
val real: real parser
val char: char parser
val int: int parser
val int32: Int32.int parser
val int64: Int64.int parser
val intInf: IntInf.int parser
val word: word parser
val word32: Word32.word parser
val word64: Word64.word parser
val option: 'a parser -> 'a option parser
val pair: 'a parser -> 'b parser -> ('a * 'b) parser
val list: 'a parser -> 'a list parser

end =
struct

infixr 6 >>=

type value = JSON.value
type object = (string * value) list
type array = value list

datatype path_element =
   Key of string
 | Index of int

type path = path_element list

local
   fun isIdentifier s =
      String.size s > 0
      andalso Char.isAlpha (String.sub (s, 0))
      andalso CharVector.all Char.isAlphaNum s

   val escape = String.translate
      (fn #"'" => "\\'"
        | #"\\" => "\\\\"
        | c => String.str c)
in
   fun formatPath (s, []) = s
     | formatPath (s, Index i :: parts) =
      formatPath ("[" ^ Int.toString i ^ "]" ^ s, parts)
     | formatPath (s, Key k :: parts) =
      if isIdentifier k
         then formatPath ("." ^ k ^ s, parts)
         else formatPath ("['" ^ escape k ^ "']" ^ s, parts)
end

datatype either = datatype Either.either

datatype 'a t = Parser of path * string list -> (path * string list, 'a) either

type 'a parser = JSON.value -> 'a t

fun unwrap (Parser p) = p

fun run (Parser q) =
   case q ([], []) of
      INR a => INR a
    | INL (path, msgs) => INL (String.concat
      ("Error in $" :: formatPath ("", path) :: ": " :: List.rev msgs))

fun runDbg tos p =
   case run p of
      INL e => print (e ^ "\n")
    | INR a => print (tos a ^ "\n")

fun pure a = Parser (fn _ => INR a)

fun fail msg = Parser (fn (path, ctx) => INL (path, msg :: ctx))

fun map f (Parser p) = Parser (fn path => Either.mapRight f (p path))

fun or (Parser p, Parser q) = Parser (fn path =>
   case p path of
      INR r => INR r
    | INL _ => q path)

fun (Parser a) >>= f = Parser (fn path =>
   case a path of
      INL e => INL e
    | INR r => unwrap (f r) path)

fun typeOf (JSON.OBJECT _) = "Object"
  | typeOf (JSON.ARRAY _) = "Array"
  | typeOf (JSON.FLOAT _) = "Number"
  | typeOf (JSON.INT _) = "Integer"
  | typeOf (JSON.BOOL _) = "Boolean"
  | typeOf (JSON.STRING _) = "String"
  | typeOf JSON.NULL = "Null"

fun typeMismatch expected actual =
   fail (String.concat ["expected ", expected, ", but encountered ", typeOf actual])

fun unexpected actual = fail ("unexpected " ^ typeOf actual)

fun prependContext name (Parser p) = Parser (fn (path, ctx) =>
   p (path, " failed, " :: name :: "parsing " :: ctx))

(**)

fun withObject _ f (JSON.OBJECT obj) = f obj
  | withObject name _ v = prependContext name (typeMismatch "Object" v)

fun withString _ f (JSON.STRING str) = f str
  | withString name _ v = prependContext name (typeMismatch "String" v)

fun withArray _ f (JSON.ARRAY arr) = f arr
  | withArray name _ v = prependContext name (typeMismatch "Array" v)

fun withBool _ f (JSON.BOOL b) = f b
  | withBool name _ v = prependContext name (typeMismatch "Boolean" v)

fun withInt _ f (JSON.INT n) = f n
  | withInt name _ v = prependContext name (typeMismatch "Integer" v)

fun withFloat _ f (JSON.FLOAT n) = f n
  | withFloat name f (JSON.INT n) =
   (f (Real.fromLargeInt n)
    handle Overflow =>
      prependContext name
         (fail ("value will cause overflow " ^ IntInf.toString n)))
  | withFloat name _ v = prependContext name (typeMismatch "Number" v)

(**)

fun addPath (Parser p, elem) = Parser (fn (path, ctx) => p (elem :: path, ctx))

fun parseField p obj key =
   case List.find (fn (k, _) => k = key) obj of
      NONE => fail (String.concat ["key ", key, " not found"])
    | SOME (_, v) => addPath (p v, Key key)

fun parseIndex p arr idx =
   addPath (p (List.nth (arr, idx)), Index idx)
   handle Subscript =>
      fail ("expected array of size at least " ^ Int.toString (idx + 1))

fun option _ JSON.NULL = pure NONE
  | option p v = map SOME (p v)

fun pair p q = withArray "(a * b)" (fn t =>
   let
      val n = List.length t
   in
      if n = 2
         then
            addPath (p (List.nth (t, 0)), Index 0) >>= (fn a =>
            addPath (q (List.nth (t, 1)), Index 1) >>= (fn b =>
            pure (a, b)))
         else fail ("cannot unpack array of length " ^ Int.toString n ^ " into a pair")
   end)

fun list p = withArray "list" (fn arr =>
   let
      fun loop (_, [], acc) = map List.rev acc
        | loop (i, x :: xs, acc) =
         addPath (p x, Index i) >>= (fn y =>
         loop (i + 1, xs, map (fn ys => y :: ys) acc))
   in
      loop (0, arr, pure [])
   end)

fun bool (JSON.BOOL b) = pure b
  | bool v = typeMismatch "Boolean" v

fun string (JSON.STRING s) = pure s
  | string v = typeMismatch "String" v

fun intInf (JSON.INT n) = pure n
  | intInf v = prependContext "IntInf.int" (typeMismatch "Integer" v)

local
   val nan: real = Real.posInf + Real.negInf
in
   fun real (JSON.INT n) = pure (Real.fromLargeInt n)
     | real (JSON.FLOAT n) = pure n
     | real (JSON.STRING "Infinity") = pure Real.posInf
     | real (JSON.STRING "+Infinity") = pure Real.posInf
     | real (JSON.STRING "-Infinity") = pure Real.negInf
     | real (JSON.STRING "NaN") = pure nan
     | real JSON.NULL = pure nan
     | real v = prependContext "real" (unexpected v)
end

val int = withInt "int" (fn n =>
   pure (IntInf.toInt n)
   handle Overflow =>
      prependContext "int"
         (fail ("value will cause overflow " ^ IntInf.toString n)))

val int64 = withInt "Int64.int" (fn n =>
   pure (Int64.fromLarge n)
   handle Overflow =>
      prependContext "Int64.int"
         (fail ("value will cause overflow " ^ IntInf.toString n)))

val int32 = withInt "Int32.int" (fn n =>
   pure (Int32.fromLarge n)
   handle Overflow =>
      prependContext "Int32.int"
         (fail ("value will cause overflow " ^ IntInf.toString n)))

local
   val maxWord = Word.toLargeInt (Word.notb 0w0)
   val maxWord32 = Word32.toLargeInt (Word32.notb 0w0)
   val maxWord64 = Word64.toLargeInt (Word64.notb 0w0)
in
   val word = withInt "word" (fn n =>
      if 0 <= n andalso n <= maxWord
         then pure (Word.fromLargeInt n)
         else prependContext "word"
            (fail ("value out of range " ^ IntInf.toString n)))
   val word32 = withInt "Word32.word" (fn n =>
      if 0 <= n andalso n <= maxWord32
         then pure (Word32.fromLargeInt n)
         else prependContext "Word32.word"
            (fail ("value out of range " ^ IntInf.toString n)))
   val word64 = withInt "Word64.word" (fn n =>
      if 0 <= n andalso n <= maxWord64
         then pure (Word64.fromLargeInt n)
         else prependContext "Word64.word"
            (fail ("value out of range " ^ IntInf.toString n)))
end

val char = withString "char" (fn s =>
   if String.size s = 1
      then pure (String.sub (s, 0))
      else prependContext "char" (fail "expected a string of length 1"))

end

structure Test =
struct
   datatype person = Person of {name: string, id: int}

   fun toString (Person {name, id}) =
      String.concat
         ["Person {name = \"", String.toString name,
          "\", id = ", Int.toString id, "}"]

   local
      open FromJSON
      infixr >>=
   in
      val parsePerson = withObject "person" (fn obj =>
         parseField string obj "name" >>= (fn name =>
         parseField int obj "id" >>= (fn id =>
         pure (Person {name = name, id = id}))))
   end
end

(* vim: set tw=0 ts=3 sw=3: *)
