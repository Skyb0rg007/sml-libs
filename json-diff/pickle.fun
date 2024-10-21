
functor PickleFn() =
struct
end

structure JSONPickle =
struct

datatype 'a t = T of {to: 'a -> JSON.value, from: JSON.value -> 'a option}

fun fromBool b = JSON.BOOL b

fun toBool JSON.NULL = SOME false
  | toBool (JSON.BOOL b) = SOME b
  | toBool _ = NONE

val bool = T {to = fromBool, from = toBool}

val INT_MAX : LargeInt.int = 2147483647
val INT_MIN : LargeInt.int = ~2147483648

fun fromIntInf n =
  if INT_MIN <= n andalso n <= INT_MAX
    then JSON.INT n
  else if n < 0
    then JSON.STRING ("-" ^ IntInf.toString (~n))
  else JSON.STRING (IntInf.toString n)

fun toIntInf (JSON.INT n) = SOME n
  | toIntInf (JSON.STRING s) = StringCvt.scanString (IntInf.scan StringCvt.DEC) s
  | toIntInf _ = NONE

val intInf = T {to = fromIntInf, from = toIntInf}

fun fromChar c = JSON.STRING (String.str c)

fun toChar (JSON.STRING s) =
  if String.size s = 1
    then SOME (String.sub (s, 0))
    else NONE
  | toChar _ = NONE

val char = T {to = fromChar, from = toChar}



end

