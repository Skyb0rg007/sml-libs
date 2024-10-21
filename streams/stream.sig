
signature STREAM =
sig

type 'a stream

val empty : 'a stream
val pure : 'a -> 'a stream

end
