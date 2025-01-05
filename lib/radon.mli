type vector = Lacaml.D.vec
type matrix = Lacaml.D.mat

module Type : sig
  type 'a t

  val float : float t
  val vector : vector t
  val matrix : matrix t

  val pair : 'a t -> 'b t -> ('a * 'b) t
end

module type Lang = sig

  type 'a exp

  val float : float -> float exp
  val vector : vector -> vector exp
  val matrix : matrix -> matrix exp

  module T2 : sig
    val fst : ('a * 'b) exp -> 'a exp
    val snd : ('a * 'b) exp -> 'b exp
  end

  val let_in : 'a exp -> ('a exp -> 'b exp) -> 'b exp
  val ( let* ) : 'a exp -> ('a exp -> 'b exp) -> 'b exp

  val neg : 'a exp -> 'a exp
  val inv : 'a exp -> 'a exp

  val add : 'a exp -> 'a exp -> 'a exp
  val sub : 'a exp -> 'a exp -> 'a exp
  val mul : 'a exp -> 'a exp -> 'a exp
  val div : 'a exp -> 'a exp -> 'a exp

  val exp : 'a exp -> 'a exp

  val vector_dot : vector exp -> vector exp -> float exp

  val diag : vector exp -> matrix exp

  type 'a obs
  val obs : ('a exp -> float exp) -> 'a Type.t -> 'a -> 'a obs
end

module Diff : Lang with type 'a obs = float * 'a
