
structure Higher:
sig
  type ('a, 'b) app
end =
struct
  type ('a, 'b) app = unit
end

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

signature NEWTYPE2 =
sig
  type ('a, 'b) s
  type t
  val inj: ('a, 'b) s -> ('a, ('b, t) Higher.app) Higher.app
  val prj: ('a, ('b, t) Higher.app) Higher.app -> ('a, 'b) s
end

structure Common =
struct
  type t = unit

  val inj = Unsafe.cast
  val prj = Unsafe.cast
end

functor Newtype0(type t) :> NEWTYPE0 where type s = t =
struct
  type s = t
  open Common
end

functor Newtype1(type 'a t) :> NEWTYPE1 where type 'a s = 'a t =
struct
  type 'a s = 'a t
  open Common
end

functor Newtype2(type ('a, 'b) t) :> NEWTYPE2 where type ('a, 'b) s = ('a, 'b) t =
struct
  type ('a, 'b) s = ('a, 'b) t
  open Common
end
