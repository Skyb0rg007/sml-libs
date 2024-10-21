
structure Test =
struct

infix 1 >>=
infix 4 <$> <$ $> <*> <* *>
infixr 6 <|>

open Parse

structure P = ParseFn(CharVector)

datatype void = Void of void

datatype sexp = Atom of string | List of sexp list

val ws: (unit, void, unit) Parse.t = many_ (char #" " <|> char #"\n")
val oparen = char #"(" *> ws
val cparen = char #")" *> ws
val ident =
   withSubstring (some_ (satisfy Char.isAlpha))
      (fn ((), s) => pure (Atom (Substring.string s)))
   <* ws
val sexp = fix (fn sexp => branch oparen (List <$> many sexp <* cparen) ident)
val src = sexp <* eof
fun runSexp str = run src () str

end

(* vim: set tw=0 ts=3 sw=3: *)
