type vector = Lacaml.D.vec
type matrix = Lacaml.D.mat

module type Lang = sig
  type 'a ty
  val tfloat : float ty
  val tvector : vector ty
  val tmatrix : matrix ty

  type 'a exp

  val float : float -> float exp
  val vector : vector -> vector exp
  val matrix : matrix -> matrix exp

  val let_in : 'a exp -> ('a exp -> 'b exp) -> 'b exp
  val ( let* ) : 'a exp -> ('a exp -> 'b exp) -> 'b exp

  val add : 'a exp -> 'a exp -> 'a exp
  val sub : 'a exp -> 'a exp -> 'a exp
  val mul : 'a exp -> 'a exp -> 'a exp

  val diag : vector exp -> matrix exp

  type 'a obs
  val obs : ('a exp -> float exp) -> ('a * 'a ty) -> 'a obs
end

module Diff : Lang with type 'a obs = float * 'a
