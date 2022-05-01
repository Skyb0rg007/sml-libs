
signature FIX =
sig
  structure F: FUNCTOR

  datatype t = Fix of t F.t
end

functor FixFn(F: FUNCTOR) :> FIX where type 'a F.t = 'a F.t =
struct
  structure F = F

  datatype t = Fix of t F.t
end
