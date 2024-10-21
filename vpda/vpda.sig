
signature VPDA_INPUT =
   sig
      type call
      type return
      type internal
   end

signature VPDA =
   sig
      structure Input: VPDA_INPUT

      type state
      type stack

      val start_state: state
      val accepting: state -> bool
      val transition_call: state * Input.call -> state * stack
      val transition_return: state * Input.return * stack option -> state
      val transition_internal: state * Input.internal -> state
   end

signature NDVPDA =
   sig
      structure Input: VPDA_INPUT

      type state
      type stack

      val all_states: state list
      val same_state: state * state -> bool

      val start_states: state list
      val accepting: state -> bool
      val transition_call: state * Input.call -> (state * stack) list
      val transition_return: state * Input.return * stack option -> state list
      val transition_internal: state * Input.internal -> state list
   end

(* vim: set tw=0 ts=3 sw=3: *)
