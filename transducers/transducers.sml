
structure Transducers =
  struct
    type ('s, 't, 'a, 'b) t = ('a, 'b) Reducer.t -> ('s, 't) Reducer.t

    fun id p = p

    (* fun take n (Reducer.R {done, init, step}) = *)
    (*   let *)
    (*     fun done' (_, s) = done s *)
    (*     fun init' () = (n, init ()) *)
    (*     fun step' (x, (n, s)) = *)
    (*       if n <= 0 *)
    (*         then (true, (n, s)) *)
    (*       else *)
    (*         let *)
    (*           val (fin, s') = step (x, s) *)
    (*         in *)
    (*           (fin, (n - 1, s')) *)
    (*         end *)
    (*   in *)
    (*     Reducer.make {done = done', init = init', step = step'} *)
    (*   end *)

    (* fun drop n (Reducer.R {done, init, step}) = *)
    (*   let *)
    (*     fun done' (_, s) = done s *)
    (*     fun init' () = (n, init ()) *)
    (*     fun step' (x, (n, s)) = *)
    (*       if n > 0 *)
    (*         then (false, (n - 1, s)) *)
    (*       else *)
    (*         let *)
    (*           val (fin, s') = step (x, s) *)
    (*         in *)
    (*           (fin, (0, s')) *)
    (*         end *)
    (*   in *)
    (*     Reducer.make {done = done', init = init', step = step'} *)
    (*   end *)
  end
