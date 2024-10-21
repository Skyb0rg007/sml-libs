
structure Types =
struct
   datatype adjust_heights_heap = AHH of {
      length: int ref,
      height_lower_bound: int ref,
      max_height_seen: int ref,
      nodes_by_height: packed_node uopt uniform_array
   }

   and internal_observer_state = Created | In_use | Disallowed | Unlinked

   and 'a internal_observer = InternalObserver of {
      state: internal_observer_state ref,
      observing: 'a node,
      on_update_handlers: 'a on_update_handler list ref,
      prev_in_all: unit (* XXX *)
   }

   and 'a node = Node of {
      id: node_id,
      state: state (* XXX *)
   }

   and state = State of {
      status: status ref,

   }

   withtype node_id = IntInf.int
end

(* vim: set ft=sml sw=3 ts=3 tw=0: *)
