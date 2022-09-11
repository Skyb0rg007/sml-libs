
functor MonomorphizeMapFn(S: MONOMORPHIZE_MAP_STRUCTS): MONO_MAP =
struct

open S

open Map

type value = value

type map = value map

end

(* vim: set tw=0 ts=3 sw=3: *)
