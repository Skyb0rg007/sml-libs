
structure Test =
struct

fun timeit k =
   let
      val timer = Timer.startCPUTimer ()
      val _ = k ()
   in
      Timer.checkCPUTimer timer
   end

end

(* vim: set ts=3 sw=3: *)
