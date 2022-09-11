
signature WORD_MAP =
sig

include MAP where type key = word

val viewMin: 'a map -> (key * 'a * 'a map) option
val viewMax: 'a map -> (key * 'a * 'a map) option

end

(* vim: set tw=0 ts=3 sw=3: *)
