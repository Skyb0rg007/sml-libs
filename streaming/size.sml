
structure Size: SIZE =
struct

datatype t =
   Exact of int
 | UpperBound of int
 | Unknown

val fromInt = Exact
val unknown = Unknown

fun toMax (Exact n) = UpperBound n
  | toMax (UpperBound n) = UpperBound n
  | toMax Unknown = Unknown

fun max (Exact m, Exact n) = Exact (Int.max (m, n))
  | max (Exact m, UpperBound n) =
   if m >= n
      then Exact m
   else UpperBound n
  | max (UpperBound m, Exact n) =
   if n >= m
      then Exact n
   else UpperBound m
  | max (UpperBound m, UpperBound n) = UpperBound (Int.max (m, n))
  | max _ = Unknown

fun min (Exact m, Exact n) = Exact (Int.min (m, n))
  | min (Exact m, UpperBound n) = UpperBound (Int.min (m, n))
  | min (Exact m, Unknown) = UpperBound m
  | min (UpperBound m, Exact n) = UpperBound (Int.min (m, n))
  | min (UpperBound m, UpperBound n) = UpperBound (Int.min (m, n))
  | min (UpperBound m, Unknown) = UpperBound m
  | min (Unknown, Exact n) = UpperBound n
  | min (Unknown, UpperBound n) = UpperBound n
  | min (Unknown, Unknown) = Unknown

fun (Exact m) + (Exact n) = Exact (Int.+ (m, n))
  | (Exact m) + (UpperBound n) = UpperBound (Int.+ (m, n))
  | (UpperBound m) + (Exact n) = UpperBound (Int.+ (m, n))
  | (UpperBound m) + (UpperBound n) = UpperBound (Int.+ (m, n))
  | _ + _ = Unknown

fun (Exact m) - (Exact n) = Exact (Int.- (m, n))
  | (Exact m) - (UpperBound _) = UpperBound m
  | (Exact m) - Unknown = UpperBound m
  | (UpperBound m) - (Exact n) = UpperBound (Int.- (m, n))
  | (UpperBound m) - (UpperBound _) = UpperBound m
  | (UpperBound m) - Unknown = UpperBound m
  | _ - _ = Unknown

fun lowerBound (Exact n) = n
  | lowerBound _ = 0

fun upperBound (Exact n) = SOME n
  | upperBound (UpperBound n) = SOME n
  | upperBound Unknown = NONE

fun exact (Exact n) = SOME n
  | exact _ = NONE

end
(* vim: set tw=0 ts=3 sw=3: *)
