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

  val vector_dot : vector exp -> vector exp -> float exp
  val diag : vector exp -> matrix exp

  type 'a obs
  val obs : ('a exp -> float exp) -> ('a * 'a ty) -> 'a obs
  val obs2 :
    ('a exp -> 'b exp -> float exp) ->
    ('a * 'a ty) ->
    ('b * 'b ty) ->
    ('a * 'b) obs
end

module Diff = struct
  open Lacaml.D

  type _ ty =
    | TFloat : float ty
    | TVector : vector ty
    | TMatrix : matrix ty

  let tfloat = TFloat
  let tvector = TVector
  let tmatrix = TMatrix

  type 'a node =
    | C of {
        ty : 'a ty ;
        v  : 'a ;
      }
    | V of {
        ty : 'a ty ;
        v : 'a ;
        mutable d : 'a
      }

  let ty = function
    | C x -> x.ty
    | V x -> x.ty

  let get_v = function
    | C x -> x.v
    | V x -> x.v

  let zero_of_value
    : type a. a ty -> a -> a
    = fun ty x ->
      match ty with
      | TFloat -> 0.
      | TVector -> Lacaml.D.Vec.(make (dim x) 0.)
      | TMatrix ->
        Lacaml.D.Mat.(make (dim1 x) (dim2 x) 0.)

  let zero n = zero_of_value (ty n) (get_v n)

  let get_d = function
    | C _ as c -> zero c
    | V u -> u.d


  let add
    : type a. a ty -> a -> a -> a
    = fun ty x y ->
      match ty with
      | TFloat -> x +. y
      | TVector -> Lacaml.D.Vec.add x y
      | TMatrix -> Lacaml.D.Mat.add x y

  let sub
    : type a. a ty -> a -> a -> a
    = fun ty x y ->
      match ty with
      | TFloat -> x -. y
      | TVector -> Lacaml.D.Vec.sub x y
      | TMatrix -> Lacaml.D.Mat.sub x y

  let neg
    : type a. a ty -> a -> a
    = fun ty x ->
      match ty with
      | TFloat -> -. x
      | TVector -> Lacaml.D.Vec.neg x
      | TMatrix -> Lacaml.D.Mat.neg x

  let mul
    : type a. a ty -> a -> a -> a
    = fun ty x y ->
      match ty with
      | TFloat -> x *. y
      | TVector -> Lacaml.D.Vec.mul x y
      | TMatrix -> Lacaml.D.Mat.mul x y

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

  let float x = fun _ -> C { ty = TFloat ; v = x }
  let vector x = fun _ -> C { ty = TVector ; v = x }
  let matrix x = fun _ -> C { ty = TMatrix ; v = x }

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
    let z = V { ty = TMatrix ; v = m ; d = zero_of_value TMatrix m } in
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
        ty = TFloat ;
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

  let obs (f : 'a exp -> float exp) (x, ty) =
    let stack = Stack.create () in
    let x = V { v = x ; ty ; d = zero_of_value ty x } in
    let y = f (Fun.const x) stack in
    update_d y 1. ;
    while not (Stack.is_empty stack) do
      (Stack.pop stack) ()
    done ;
    get_v y, get_d x

  let obs2 (f : 'a exp -> 'b exp -> float exp) (x, x_ty) (y, y_ty) =
    let stack = Stack.create () in
    let x = V { v = x ; ty = x_ty ; d = zero_of_value x_ty x } in
    let y = V { v = y ; ty = y_ty ; d = zero_of_value y_ty y } in
    let z = f (Fun.const x) (Fun.const y) stack in
    update_d z 1. ;
    while not (Stack.is_empty stack) do
      (Stack.pop stack) ()
    done ;
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
      (3., tfloat)
  )
  |> [%show: float * float]
  |> print_endline ;
  [%expect {| (46., 27.) |}]

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
      (Lacaml.D.Vec.of_array [|1.;2.;0.;1.|], tvector)
      (2., tfloat)
  )
  |> [%show: float * (vector * float)]
  |> print_endline ;
  [%expect {||}]
