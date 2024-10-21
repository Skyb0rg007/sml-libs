
structure Note =
struct

datatype step = Step of {
    srcs : srcs ref,
    delayed : srcs ref,
    cleanup : (unit -> unit) list ref
  }

and 'a cell = Cell of {
    eq : 'a * 'a -> bool,
    stamp : step ref,
    value : 'a ref,
    changed : bool ref,
    srcs : srcs list ref,
    srcs_changed : bool ref,
    update : (step * 'a cell -> unit) ref
  }

and src = Src of {
    id : int
  }

withtype srcs = src IntRedBlackMap.map

end
