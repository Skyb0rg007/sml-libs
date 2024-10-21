
structure U = Universal

structure Higher =
   struct
      datatype ('a, 'b) app = App of U.universal * 'a U.tag
   end

signature NEWTYPE1 =
   sig
      type 'a s
      type t

      val inject: 'a s -> ('a, t) Higher.app
      val project: ('a, t) Higher.app -> 'a s
   end

signature NEWTYPE2 =
   sig
      type ('a, 'b) s
      type t

      val inject: ('a, 'b) s -> ('a, ('b, t) Higher.app) Higher.app
      val project: ('a, ('b, t) Higher.app) Higher.app -> ('a, 'b) s
   end

(****)

signature NEWTYPE1_STRUCTS =
   sig
      type 'a t

      val mapIso: ('a, 'b) Iso.t -> 'a t -> 'b t
   end

functor Newtype1(S: NEWTYPE1_STRUCTS) :> NEWTYPE1 where type 'a s = 'a S.t =
   struct
      type 'a s = 'a S.t
      type t = unit

      val tag: U.universal s U.tag = U.tag ()

      fun inject x =
         let
            val t = U.tag ()
         in
            Higher.App (U.tagInject tag (S.mapIso (Iso.fromTag t) x), t)
         end

      fun project (Higher.App (y, t)) =
         S.mapIso (Iso.invert (Iso.fromTag t)) (U.tagProject tag y)
   end

signature NEWTYPE2_STRUCTS =
   sig
      type ('a, 'b) t

      val mapIso1: ('a, 'c) Iso.t -> ('a, 'b) t -> ('c, 'b) t
      val mapIso2: ('b, 'c) Iso.t -> ('a, 'b) t -> ('a, 'c) t
   end

functor Newtype2(S: NEWTYPE2_STRUCTS) :> NEWTYPE2 where type ('a, 'b) s = ('a, 'b) S.t =
   struct
      type ('a, 'b) s = ('a, 'b) S.t
      type t = U.universal

      structure T = Newtype1(
         struct
            type 'a t = (U.universal, 'a) s

            val mapIso = S.mapIso2
         end)

      val tag: (U.universal, U.universal) s U.tag = U.tag ()

      fun inject (x: ('a, 'b) s): ('a, ('b, t) Higher.app) Higher.app =
         let
            val t: 'a U.tag = U.tag ()
            val iso = Iso.fromTag t

            val y: (U.universal, 'b) s = S.mapIso1 iso x
            val z: ('b, T.t) Higher.app = T.inject y
         in
            Higher.App (raise Fail "", t)
         end

      fun project _ = raise Fail ""
   end

structure H =
struct
  datatype ('a, 'b) app =
    App of U.universal * 'a U.tag
end

functor N1(type 'a t val iso: ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t):
sig
  type 'a s = 'a t
  type t

  val inj: 'a s -> ('a, t) H.app
  val prj: ('a, t) H.app -> 'a s
end =
struct
  type 'a s = 'a t
  type t = unit

  val tag: U.universal s U.tag = U.tag ()

  fun inj (x : 'a s): ('a, t) H.app =
    let
      val t = U.tag ()
      val y = iso (U.tagInject t) (U.tagProject t) x
    in
      H.App (U.tagInject tag y, t)
    end

  fun prj (H.App (x, t)) =
    iso (U.tagProject t) (U.tagInject t) (U.tagProject tag x)
end

functor N2(
    type ('a, 'b) t
    val iso1: ('a -> 'c) -> ('c -> 'a) -> ('a, 'b) t -> ('c, 'b) t
    val iso2: ('b -> 'c) -> ('c -> 'b) -> ('a, 'b) t -> ('a, 'c) t):
  sig
    type ('a, 'b) s = ('a, 'b) t
    type t
  end =
struct
  type ('a, 'b) s = ('a, 'b) t
  type t = unit

  val tag: (U.universal, U.universal) s U.tag = U.tag ()
end

structure Higher :>
sig
  type ('a, 'b) app

  val fromExn: exn -> ('a, 'b) app
  val toExn: ('a, 'b) app -> exn
end =
struct
  type ('a, 'b) app = exn

  fun fromExn x = x
  fun toExn x = x
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

functor Newtype0(type t) :> NEWTYPE0 where type s = t =
struct
  type t = t
  type s = t
  fun inj x = x
  fun prj x = x
end

(* functor Newtype1(type 'a t) :> NEWTYPE1 where type 'a s = 'a t = *)
(* struct *)
(*   type 'a s = 'a t *)
(*   type t = unit *)

(*   (1* datatype ('a, 'b) app = *1) *)
(*   (1*   App of 'a s * ('b -> t) * (t -> 'b) *1) *)

(*   (1* exception E of 'a t *1) *)
(* end *)

(* functor Newtype2(type ('a, 'b) t) :> NEWTYPE2 where type ('a, 'b) s = ('a, 'b) t = *)
(* struct *)
(*   type ('a, 'b) s = ('a, 'b) t *)
(* end *)

(* vim: set tw=0 ts=3 sw=3: *)
