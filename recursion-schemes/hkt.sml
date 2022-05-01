
structure HKT:
sig
  type ('p, 'f) app

  val app: Universal.universal -> ('a, 'b) app
  val unApp: ('a, 'b) app -> Universal.universal

  type ('a, 'b) coercion = {
    inj: 'a -> 'b,
    prj: 'b -> 'a
  }
end =
struct
  datatype ('p, 'f) app = App of Universal.universal

  val app = App
  fun unApp (App x) = x

  type ('a, 'b) coercion = {
    inj: 'a -> 'b,
    prj: 'b -> 'a
  }
end

structure Common =
struct
  type t = Universal.universal

  fun coerce () =
    let
      val tag = Universal.tag ()
    in
      { inj = HKT.app o Universal.tagInject tag,
        prj = Universal.tagProject tag o HKT.unApp }
    end
end

signature Newtype0 =
sig
  type s
  type t

  val coerce: (s, t) HKT.coercion
end

signature Newtype1 =
sig
  type 'a s
  type t

  val coerce: unit -> ('a s, ('a, t) HKT.app) HKT.coercion
end

signature Newtype2 =
sig
  type ('a, 'b) s
  type t

  val coerce: unit -> (('a, 'b) s, ('a, ('b, t) HKT.app) HKT.app) HKT.coercion
end

signature Newtype3 =
sig
  type ('a, 'b, 'c) s
  type t

  val coerce: unit -> (('a, 'b, 'c) s, ('a, ('b, ('c, t) HKT.app) HKT.app) HKT.app) HKT.coercion
end


functor Newtype0(type t) :> Newtype0 where type s = t =
struct
  type s = t
  type t = Universal.universal

  val tag: s Universal.tag = Universal.tag ()
  val inj = Universal.tagInject tag
  val prj = Universal.tagProject tag
  val coerce = { inj = inj, prj = prj }
end

functor Newtype1(type 'a t) :> Newtype1 where type 'a s = 'a t =
struct
  type 'a s = 'a t
  open Common
end

functor Newtype2(type ('a, 'b) t) :> Newtype2 where type ('a, 'b) s = ('a, 'b) t =
struct
  type ('a, 'b) s = ('a, 'b) t
  open Common
end

functor Newtype3(type ('a, 'b, 'c) t) :> Newtype3 where type ('a, 'b, 'c) s = ('a, 'b, 'c) t =
struct
  type ('a, 'b, 'c) s = ('a, 'b, 'c) t
  open Common
end
