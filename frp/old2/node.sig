
signature NODE =
sig

type node

val none: Universal.universal
val create:
   {compute: node -> Universal.universal, dependencies: unit -> node list}
   -> node

val value: node -> Universal.universal
val setValue: node * Universal.universal -> unit
val refcount: node -> int
val annotate: node * string -> unit
val name: node -> string
val isChangingInCurrentStabilization: node -> bool

structure Q:
   sig
      val add: node -> bool
      val removeMin: unit -> node option
      val drain: (node -> unit) -> unit
   end

end

(* vim: set ft=sml tw=0 ts=3 sw=3: *)
