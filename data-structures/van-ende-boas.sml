
structure VanEmdeBoas =
struct
   structure U = Word32

   fun high (x, n) = Word.>> (x, n)

   fun low (x, n) = Word.andb (x, Word.<< (0w1, n) - 0w1)


end

(* vim: set ft=sml ts=3 sw=3 tw=0: *)
