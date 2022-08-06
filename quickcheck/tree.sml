
structure Tree :> TREE =
struct
   datatype 'a t = T of 'a * 'a t Seq.t

   val make = T

   fun root (T (x, _)) = x

   fun children (T (_, xs)) = xs

   fun map f (T (x, xs)) = T (f x, Seq.map (map f) xs)

   fun ap (T (f, fs), T (x, xs)) =
      T (f x, Seq.append 
         (Seq.map (fn f' => ap (f', T (x, xs))) fs,
          Seq.map (fn x' => ap (T (f, fs), x')) xs))

   fun curry f x y = f (x, y)

   fun map2 f (x, y) = ap (map (curry f) x, y)

   fun bind (T (x, xs)) f =
      let
         val T (y, ys) = f x
      in
         T (y, Seq.append (Seq.map (fn x' => bind x' f) xs, ys))
      end

   fun pure x = T (x, Seq.empty)

   fun opt (T (x, xs)) = T (SOME x, Seq.cons (pure NONE, Seq.map opt xs))

   fun sequence [] = pure []
     | sequence (t :: ts) = map2 op :: (t, sequence ts)

   fun filterShrinks p (T (x, xs)) =
      let
         fun f t =
            if p (root t)
               then SOME (filterShrinks p t)
            else NONE
      in
         T (x, Seq.mapPartial f xs)
      end

   fun applicativeTake (0, _) = pure []
     | applicativeTake (_, []) = pure []
     | applicativeTake (n, t :: ts) = map2 op :: (t, applicativeTake (n - 1, ts))

   fun makePrimitive shrink x =
      T (x, Seq.map (makePrimitive shrink) (shrink x))
end

(* vim: set tw=0 ts=3 sw=3: *)
