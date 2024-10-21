
structure AbstractBits =
struct

infix << >> andb orb

fun reverse x =
   let
      open Word64
      val x = ((x andb 0wxaaaaaaaaaaaaaaaa) >> 0w1)
          orb ((x andb 0wx5555555555555555) << 0w1)
      val x = ((x andb 0wxcccccccccccccccc) >> 0w2)
          orb ((x andb 0wx3333333333333333) << 0w2)
      val x = ((x andb 0wxf0f0f0f0f0f0f0f0) >> 0w4)
          orb ((x andb 0wx0f0f0f0f0f0f0f0f) << 0w4)
      val x = ((x andb 0wxff00ff00ff00ff00) >> 0w8)
          orb ((x andb 0wx00ff00ff00ff00ff) << 0w8)
      val x = ((x andb 0wxffff0000ffff0000) >> 0w16)
          orb ((x andb 0wx0000ffff0000ffff) << 0w16)
   in
      (x >> 0w32) orb (x << 0w32)
   end

datatype t = T of { ones: Word64.word, zeros: Word64.word }

fun add (T {ones = o1, zeros = z1}, T {ones = o2, zeros = z2}) =
   let
      val (ones, zeros) = (0w0, 0w0)
   in
      T {ones = ones, zeros = zeros}
   end

end

(* vim: set ts=3 sw=3: *)
