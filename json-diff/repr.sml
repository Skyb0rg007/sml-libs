
structure Repr =
struct

datatype t
  = BOOL of bool
  | INTEGER of LargeInt.int
  | WORD of LargeWord.word
  | CHAR of char
  | REAL of LargeReal.real
  | STRING of substring

end
