
signature NEWTYPE0 =
sig
  type s
  type t
  val inj: s -> t
  val prj: t -> s
end

signature NEWTYPE1 =
sig
  type 'a s
  type t
  val inj: 'a s -> ('a, t) Higher.app
  val prj: ('a, t) Higher.app -> 'a s
end
