
signature NODE =
sig
   structure M: MONOID

   datatype 'a t =
      Node2 of M.t * 'a * 'a
    | Node3 of M.t * 'a * 'a * 'a

   val node2: ('a -> M.t) -> 'a * 'a -> 'a t

   val node3: ('a -> M.t) -> 'a * 'a * 'a -> 'a t

   val measure: 'a t -> M.t

   val toDigit: 'a t -> 'a Digit.t

   val map: ('b -> M.t) -> ('a -> 'b) -> 'a t -> 'b t

   val mapWithPos:
      ('a -> M.t)
      -> ('b -> M.t)
      -> (M.t * 'a -> 'b)
      -> M.t * 'a t
      -> 'b t

   val search:
      ('a -> M.t)
      -> (M.t * M.t -> bool)
      -> M.t * 'a t * M.t
      -> 'a Digit.t option * 'a * 'a Digit.t option

   val split:
      ('a -> M.t)
      -> (M.t -> bool)
      -> M.t * 'a t
      -> 'a Digit.t option * 'a * 'a Digit.t option
end
(* vim: set ft=sml tw=0 ts=3 sw=3: *)
