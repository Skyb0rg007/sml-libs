
module Transducer = struct

    type ('a, 'b) chan = Chan of ('a * ('b, 'a) chan) -> unit

end

module Hyper = struct

    type ('a, 'b) t = Hyper of 'a * ('b, 'a) t



end
