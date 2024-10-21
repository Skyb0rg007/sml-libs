
structure Iso: ISO =
struct

datatype ('a, 'b) t = T of ('a -> 'b) * ('b -> 'a)

fun invert (T (f, g)) = T (g, f)

fun inject (T (f, _), a) = f a

fun project (T (_, g), b) = g b

fun make f g = T (f, g)

fun fromTag t = T (Universal.tagInject t, Universal.tagProject t)

end

(* vim: set tw=0 ts=3 sw=3: *)
