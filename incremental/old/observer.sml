
structure Observer: OBSERVER =
struct

datatype 'a t = T of {id: unit ref, onChange: 'a -> unit}

fun new f = T {id = ref (), onChange = f}

fun observe (T {onChange, ...}, x) = onChange x

fun equals (T {id = id1, ...}, T {id = id2, ...}) = id1 = id2

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
