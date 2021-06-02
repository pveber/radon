type vector
type matrix
type 'a ty

module DSL : sig
  val tfloat : float ty
  val tvector : vector ty
  val tmatrix : matrix ty
  val tpair : 'a ty -> 'b ty -> ('a * 'b) ty

  type 'a exp
  val float : float -> float exp

  val pair : 'a exp -> 'b exp -> ('a * 'b) exp
  val fst : ('a * 'b) exp -> 'a exp
  val snd : ('a * 'b) exp -> 'b exp

  val add : 'a ty -> 'a exp -> 'a exp -> 'a exp
  val cos : float exp -> float exp
  val diag : vector exp -> matrix exp
  val exp : float exp -> float exp
  val inv : float exp -> float exp
  val log : float exp -> float exp
  val mul : float exp -> float exp -> float exp
  val matrix_dot : matrix exp -> matrix exp -> matrix exp
  val matrix_dotv : matrix exp -> vector exp -> vector exp
  val matrix_exp : matrix exp -> matrix exp
  val matrix_expm : matrix exp -> matrix exp
  val matrix_row_sums : matrix exp -> vector exp
  val matrix_scale : float exp -> matrix exp -> matrix exp
  val matrix_sub : matrix exp -> matrix exp -> matrix exp
  val sin : float exp -> float exp
  val vector_dot : vector exp -> vector exp -> float exp
  val vector_exp : vector exp -> vector exp
  val vector_mul : vector exp -> vector exp -> vector exp
  val vector_scale : float exp -> vector exp -> vector exp
  val vector_sum : vector exp -> float exp

  val eval :
    ('a exp -> float exp) ->
    'a ty ->
    'a ->
    float
    
  val grad :
    ('a exp -> float exp) ->
    'a ty ->
    'a ->
    float * 'a
end

