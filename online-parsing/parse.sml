
functor ParseFn(M: MONOID) =
struct

datatype 'a t =
   Failure of string
 | Result of M.t * 'a
 | Partial of ('a -> 'a) * 'a t * (M.t -> 'a t)
 | Delay of 'a t * (M.t -> 'a t)
 | Choice of 'a t * 'a t

fun feedEof (p as Failure _) = p
  | feedEof (p as Result _) = p
  | feedEof (Partial (r, e, _)) = prepend r (feedEof e)

and prepend _ (p as Failure _) = p
  | prepend r1 (Result (s, r2)) = Result (s, r1 r2)

and partial (_, Failure _) = raise Fail "Internal error"
  | partial (f, Result (s, r)) = Result (s, f r)
  | partial (r1, Partial (r2, e, f)) = Partial (r1 o r2, e, f)
  | partial (r, p) = Partial (r, feedEof p, fn s => feed s p)

and feed s (Failure msg) = Failure msg
  | feed s (Result (s', r)) = Result (M.+ (s', s), r)
  | feed s (Partial (r, _, f)) = partial (r, f s)

end

(* vim: set tw=0 ts=3 sw=3: *)
