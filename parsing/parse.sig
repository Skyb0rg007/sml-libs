
signature PARSE =
sig

type ('s, 'e, 'a) t
datatype ('s, 'e, 'a) result = Ok of 'a * 's * substring | Fail | Err of 'e

val run: ('s, 'e, 'a) t -> 's -> string -> ('s, 'e, 'a) result

val get: ('s, 'e, 's) t
val put: 's -> ('s, 'e, unit) t
val modify: ('s -> 's) -> ('s, 'e, unit) t

val map: ('a -> 'b) -> ('s, 'e, 'a) t -> ('s, 'e, 'b) t
val <$> : ('a -> 'b) * ('s, 'e, 'a) t -> ('s, 'e, 'b) t
val <$ : 'b * ('s, 'e, 'a) t -> ('s, 'e, 'b) t
val $> : ('s, 'e, 'a) t * 'b -> ('s, 'e, 'b) t

val pure: 'a -> ('s, 'e, 'a) t
val <*> : ('s, 'e, 'a -> 'b) t * ('s, 'e, 'a) t -> ('s, 'e, 'b) t
val *> : ('s, 'e, 'a) t * ('s, 'e, 'b) t -> ('s, 'e, 'b) t
val <* : ('s, 'e, 'a) t * ('s, 'e, 'b) t -> ('s, 'e, 'a) t

val >>= : ('s, 'e, 'a) t * ('a -> ('s, 'e, 'b) t) -> ('s, 'e, 'b) t

val fix: (('s, 'e, 'a) t -> ('s, 'e, 'a) t) -> ('s, 'e, 'a) t

val empty: ('s, 'e, 'a) t
val err: 'e -> ('s, 'e, 'a) t
val withError: ('s, 'e, 'a) t -> ('e -> ('s, 'e, 'a) t) -> ('s, 'e, 'a) t
val lookahead: ('s, 'e, 'a) t -> ('s, 'e, 'a) t
val fails: ('s, 'e, 'a) t -> ('s, 'e, unit) t
val try: ('s, 'e, 'a) t -> ('s, 'e, 'a) t
val optional: ('s, 'e, 'a) t -> ('s, 'e, 'a option) t
val many: ('s, 'e, 'a) t -> ('s, 'e, 'a list) t
val many_: ('s, 'e, 'a) t -> ('s, 'e, unit) t
val some_: ('s, 'e, 'a) t -> ('s, 'e, unit) t
val eof: ('s, 'e, unit) t
val take: int -> ('s, 'e, substring) t
val anyChar: ('s, 'e, char) t
val char: char -> ('s, 'e, unit) t
val string: string -> ('s, 'e, unit) t
val satisfy: (char -> bool) -> ('s, 'e, unit) t
val anyWChar: ('s, 'e, word) t

val withSubstring: ('s, 'e, 'a) t -> ('a * substring -> ('s, 'e, 'b) t) -> ('s, 'e, 'b) t

val <|> : ('s, 'e, 'a) t * ('s, 'e, 'a) t -> ('s, 'e, 'a) t
val branch : ('s, 'e, 'a) t -> ('s, 'e, 'b) t -> ('s, 'e, 'b) t -> ('s, 'e, 'b) t

end

(* vim: set tw=0 ts=3 sw=3: *)
