
structure Propogate =
struct

datatype 'a t = Mod of {
   value: 'a ref,
   readers: rnode list ref
}

and rnode = RNode of {
   pending_update: bool ref
}

(* structure SPNode = *)
(*    struct *)
(*       datatype t = T of { *)
(*          tag: tag, *)
(*          parent: t option ref, *)
(*          left: t option ref, *)
(*          right: t option ref, *)
(*          dirty: bool ref *)
(*       } *)
(*       and tag = S | P | R *)

(*       fun isDirty (T {dirty, ...}) = !dirty *)

(*       fun makeDirty (T {tag, parent, left, right, dirty}) = *)
(*          (dirty := true *)
(*           ; case !parent of *)
(*                NONE => () *)
(*              | SOME p => *)
(*                   if not (isDirty p) *)
(*                      then makeDirty p *)
(*                   else ()) *)

(*       fun propogate (T {tag = S, parent, left, right, dirty}) = *)
(*          if !dirty *)
(*             then *)
(*                (case !left of NONE => () | SOME l => propogate l *)
(*                 ; case !right of NONE => () | SOME r => propogate r *)
(*                 ; dirty := false) *)
(*          else () *)
(*         | propogate (T {tag = R, parent, left, right, dirty}) = *)
(*          if !dirty *)
(*             then *)
(*                (case !left of NONE => () | SOME l => propogate l *)
(*                 ; case !right of NONE => () | SOME r => propogate r *)
(*                 ; dirty := false) *)
(*          else () *)
(*    end *)

end

(* vim: set tw=0 ts=3 sw=3: *)
