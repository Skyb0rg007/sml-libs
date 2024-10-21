
structure Test =
struct

  structure U = Unfold
  structure F = Fold

  val sum = F.foldl Int.+ 0
  val nats = U.unfoldr (fn n => SOME (n, n + 1))

  fun sumUpTo n = F.drive (U.takeWhile (fn m => m < n) nats) sum 0
end
