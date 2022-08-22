
functor MonomorphizeMapFn(structure Map: MAP type value) :>
   MONO_MAP where type key = Map.key
              and type value = value =
struct
   open Map

   type value = value
   type map = value map
end

(* vim: set tw=0 ts=3 sw=3: *)
