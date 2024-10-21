
structure Note =
struct

structure U = Universal

datatype src = Src of {
   id: int,
   cell: cell
}

and cell = Cell of {
   eq: (U.universal * U.universal -> bool) ref,
   stamp: step ref,
   srcs: srcs ref,
   srcs_changed: bool ref,
   value: U.universal ref,
   value_changed: U.universal ref,
   update: (step * cell -> unit) ref
}

and step = Step of {
   srcs: srcs ref,
   delayed: srcs ref,
   cleanup: (unit -> unit) list ref
}

and srcs = Srcs of src list

withtype 'a src_typed = src * 'a U.tag
and 'a cell_typed = cell * 'a U.tag

structure Cell:
   sig
      type 'a t = 'a cell_typed
      type untyped = cell
   end =
   struct
      type 'a t = 'a cell_typed
      datatype untyped = datatype cell

      fun create eq step srcs value update =
         Cell {eq = eq, step = step}
   end

end
(* vim: set tw=0 ts=3 sw=3: *)
