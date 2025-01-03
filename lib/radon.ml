type vector = Lacaml.D.vec

let pp_vector = Lacaml.D.pp_vec

type matrix = Lacaml.D.mat

module Vector = struct
  open Lacaml.D

  let inplace_scale f a = scal f a

  let scale f a =
    let r = copy a in
    inplace_scale f r ;
    r
end

module Type = struct
  type _ t =
    | Float : float t
    | Vector : vector t
    | Matrix : matrix t
    | Tuple2 : 'a t * 'b t -> ('a * 'b) t

  let float = Float
  let vector = Vector
  let matrix = Matrix
  let t2 x y = Tuple2 (x, y)
  [@@ocaml.warning "-32"]
end

module type Lang = sig
  type 'a exp

  val float : float -> float exp
  val vector : vector -> vector exp
  val matrix : matrix -> matrix exp

  val let_in : 'a exp -> ('a exp -> 'b exp) -> 'b exp
  val ( let* ) : 'a exp -> ('a exp -> 'b exp) -> 'b exp

  val add : 'a exp -> 'a exp -> 'a exp
  val sub : 'a exp -> 'a exp -> 'a exp
  val mul : 'a exp -> 'a exp -> 'a exp

  val vector_dot : vector exp -> vector exp -> float exp
  val diag : vector exp -> matrix exp

  type 'a obs
  val obs : ('a exp -> float exp) -> 'a Type.t -> 'a -> 'a obs
  val obs2 :
    ('a exp -> 'b exp -> float exp) ->
    ('a Type.t * 'a) ->
    ('b Type.t * 'b) ->
    ('a * 'b) obs
end

module Diff = struct
  open Lacaml.D

  type 'a node =
    | C of {
        ty : 'a Type.t ;
        v  : 'a ;
      }
    | V of {
        ty : 'a Type.t ;
        v : 'a ;
        mutable d : 'a
      }

  let ty = function
    | C x -> x.ty
    | V x -> x.ty

  let get_v = function
    | C x -> x.v
    | V x -> x.v

  let rec zero_of_value
    : type a. a Type.t -> a -> a
    = fun ty x ->
      match ty with
      | Type.Float -> 0.
      | Type.Vector -> Lacaml.D.Vec.(make (dim x) 0.)
      | Type.Matrix ->
        Lacaml.D.Mat.(make (dim1 x) (dim2 x) 0.)
      | Type.Tuple2 (ty1, ty2) ->
        (zero_of_value ty1 (fst x), zero_of_value ty2 (snd x))

  let zero n = zero_of_value (ty n) (get_v n)

  let get_d = function
    | C _ as c -> zero c
    | V u -> u.d


  let rec add
    : type a. a Type.t -> a -> a -> a
    = fun ty x y ->
      match ty with
      | Type.Float -> x +. y
      | Type.Vector -> Lacaml.D.Vec.add x y
      | Type.Matrix -> Lacaml.D.Mat.add x y
      | Type.Tuple2 (t1, t2) ->
        (add t1 (fst x) (fst y),
         add t2 (snd x) (snd y))

  let rec sub
    : type a. a Type.t -> a -> a -> a
    = fun ty x y ->
      match ty with
      | Type.Float -> x -. y
      | Type.Vector -> Lacaml.D.Vec.sub x y
      | Type.Matrix -> Lacaml.D.Mat.sub x y
      | Type.Tuple2 (t1, t2) ->
        (sub t1 (fst x) (fst y),
         sub t2 (snd x) (snd y))

  let rec neg
    : type a. a Type.t -> a -> a
    = fun ty x ->
      match ty with
      | Type.Float -> -. x
      | Type.Vector -> Lacaml.D.Vec.neg x
      | Type.Matrix -> Lacaml.D.Mat.neg x
      | Type.Tuple2 (t1, t2) ->
        (neg t1 (fst x), neg t2 (snd x))

  let rec mul
    : type a. a Type.t -> a -> a -> a
    = fun ty x y ->
      match ty with
      | Type.Float -> x *. y
      | Type.Vector -> Lacaml.D.Vec.mul x y
      | Type.Matrix -> Lacaml.D.Mat.mul x y
      | Type.Tuple2 (t1, t2) ->
        (mul t1 (fst x) (fst y),
         mul t2 (snd x) (snd y))

  let update_d node x = match node with
    | C _ -> ()
    | V v -> v.d <- add v.ty v.d x

  let update_vec_d node f = match node with
    | C _ -> ()
    | V v ->
      for i = 1 to Vec.dim v.d do
        v.d.{i} <- v.d.{i} +. f (i - 1)
      done

  type 'a exp = (unit -> unit) Stack.t -> 'a node

  let float x = fun _ -> C { ty = Type.Float ; v = x }
  let vector x = fun _ -> C { ty = Type.Vector ; v = x }
  let matrix x = fun _ -> C { ty = Type.Matrix ; v = x }

  let add (xf : 'a exp) (yf : 'a exp) stack =
    let x = xf stack in
    let y = yf stack in
    let z = V {
        ty = ty x ;
        v = add (ty x) (get_v x) (get_v y) ;
        d = zero x
      } in
    let backward () =
      update_d x (get_d z) ;
      update_d y (get_d z)
    in
    Stack.push backward stack ;
    z

  let sub (xf : 'a exp) (yf : 'a exp) stack =
    let x = xf stack in
    let y = yf stack in
    let ty = ty x in
    let z = V {
        ty ;
        v = sub ty (get_v x) (get_v y) ;
        d = zero x
      } in
    let backward () =
      update_d x (get_d z) ;
      update_d y (neg ty (get_d z))
    in
    Stack.push backward stack ;
    z

  let let_in (xf : 'a exp) (f : 'a exp -> 'b exp) stack =
    let x = xf stack in
    let y = f (Fun.const x) stack in
    y

  let (let*) = let_in

  let mul (xf : 'a exp) (yf : 'a exp) stack =
    let x = xf stack in
    let y = yf stack in
    let ty = ty x in
    let z = V {
        ty ;
        v = mul ty (get_v x) (get_v y) ;
        d = zero x
      } in
    let backward () =
      update_d x (mul ty (get_d z) (get_v y)) ;
      update_d y (mul ty (get_d z) (get_v x))
    in
    Stack.push backward stack ;
    z

  let diag xf stack =
    let x_node = xf stack in
    let x = get_v x_node in
    let n = Vec.dim x in
    let m = Mat.init_cols n n (fun i j -> if i = j then x.{i} else 0.) in
    let z = V { ty = Type.Matrix ; v = m ; d = zero_of_value Type.Matrix m } in
    let backward () =
      let dz = get_d z in
      update_vec_d x_node (fun i -> dz.{i, i})
    in
    Stack.push backward stack ;
    z

  let vector_dot xf yf stack =
    let x = xf stack in
    let y = yf stack in
    let z = V {
        ty = Type.Float ;
        v = Lacaml.D.dot (get_v x) (get_v y) ;
        d = 0.
      } in
    let backward () =
      let dz = get_d z in
      update_d x (Vector.scale dz (get_v y)) ;
      update_d y (Vector.scale dz (get_v x))
    in
    Stack.push backward stack ;
    z

  type 'a obs = float * 'a

  let backward stack z =
    update_d z 1. ;
    while not (Stack.is_empty stack) do
      (Stack.pop stack) ()
    done

  let parameter ty init =
    V { v = init ; ty ; d = zero_of_value ty init }

  let obs (f : 'a exp -> float exp) ty x =
    let stack = Stack.create () in
    let x = parameter ty x in
    let y = f (Fun.const x) stack in
    backward stack y ;
    get_v y, get_d x

  let obs2 (f : 'a exp -> 'b exp -> float exp) (x_ty, x) (y_ty, y) =
    let stack = Stack.create () in
    let x = parameter x_ty x in
    let y = parameter y_ty y in
    let z = f (Fun.const x) (Fun.const y) stack in
    backward stack z ;
    get_v z, (get_d x, get_d y)
end

let%expect_test "second order polynomial (2x + 1)^2 - x" =
  print_endline {|
f(x) = (2x + 1)^2 - x evaluated in 3
f'(x) = 4(2x + 1) - 1
|} ;
  Diff.(
    obs
      (fun x ->
         let* y = add (mul (float 2.) x) (float 1.) in
         sub (mul y y) x)
      Type.float
      3.
  )
  |> [%show: float * float]
  |> print_endline ;
  [%expect {|
    f(x) = (2x + 1)^2 - x evaluated in 3
    f'(x) = 4(2x + 1) - 1

    (46., 27.) |}]

let%expect_test "dot product w^T w + b" =
  print_endline {|
>>> import torch
>>> w = torch.tensor([1.0,2,0,1], requires_grad = True)
>>> b = torch.tensor(2.0, requires_grad = True)
>>> z = torch.dot(w, w) + b
>>> z.backward()
>>> w.grad
tensor([2., 4., 0., 2.])
>>> b.grad
tensor(1.)
|} ;
  Diff.(
    obs2
      (fun w b -> add (vector_dot w w) b)
      (Type.vector, Lacaml.D.Vec.of_array [|1.;2.;0.;1.|])
      (Type.float, 2.)
  )
  |> [%show: float * (vector * float)]
  |> print_endline ;
  [%expect {|
    >>> import torch
    >>> w = torch.tensor([1.0,2,0,1], requires_grad = True)
    >>> b = torch.tensor(2.0, requires_grad = True)
    >>> z = torch.dot(w, w) + b
    >>> z.backward()
    >>> w.grad
    tensor([2., 4., 0., 2.])
    >>> b.grad
    tensor(1.)

    (8., (2
          4
          0
          2, 1.)) |}]
