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
  val obs2 :
    ('a exp -> 'b exp -> float exp) ->
    ('a Type.t * 'a) ->
    ('b Type.t * 'b) ->
    ('a * 'b) obs
end

module Ops = struct
  let rec zero
    : type a. a Type.t -> a -> a
    = fun ty x ->
      match ty with
      | Type.Float -> 0.
      | Type.Vector -> Lacaml.D.Vec.(make (dim x) 0.)
      | Type.Matrix ->
        Lacaml.D.Mat.(make (dim1 x) (dim2 x) 0.)
      | Type.Tuple2 (ty1, ty2) ->
        (zero ty1 (fst x), zero ty2 (snd x))

  (* let rec one *)
  (*   : type a. a Type.t -> a -> a *)
  (*   = fun ty _x -> *)
  (*     match ty with *)
  (*     | Type.Float -> 1. *)
  (*     | Type.Vector *)
  (*     | Type.Matrix *)
  (*     | Type.Tuple2 _ -> assert false *)

  let rec unary_operation
    : type a.
      (float -> float) ->
      (vector -> vector) ->
      (matrix -> matrix) ->
      a Type.t -> a -> a
    = fun f0 f1 f2 ty x ->
      match ty with
      | Type.Float -> f0 x
      | Type.Vector -> f1 x
      | Type.Matrix -> f2 x
      | Type.Tuple2 (t1, t2) ->
        (unary_operation f0 f1 f2 t1 (fst x),
         unary_operation f0 f1 f2 t2 (snd x))

  let neg ty x =
    unary_operation
      (fun x -> -. x)
      Lacaml.D.Vec.neg
      Lacaml.D.Mat.neg
      ty x

  let inv ty x =
    let inv x = 1. /. x in
    unary_operation
      inv
      Lacaml.D.Vec.(map inv)
      Lacaml.D.Mat.(map inv)
      ty x

  let exp ty x =
    unary_operation
      Float.exp
      Lacaml.D.Vec.exp
      Lacaml.D.Mat.exp
      ty x

  let rec binary_operation
    : type a.
      (float -> float -> float) ->
      (vector -> vector -> vector) ->
      (matrix -> matrix -> matrix) ->
      a Type.t -> a -> a -> a
    = fun f0 f1 f2 ty x y ->
      match ty with
      | Type.Float -> f0 x y
      | Type.Vector -> f1 x y
      | Type.Matrix -> f2 x y
      | Type.Tuple2 (t1, t2) ->
        (binary_operation f0 f1 f2 t1 (fst x) (fst y),
         binary_operation f0 f1 f2 t2 (snd x) (snd y))

  let add ty x y =
    binary_operation ( +. )
      (fun x y -> Lacaml.D.Vec.add x y)
      (fun x y -> Lacaml.D.Mat.add x y)
      ty x y

  let sub ty x y =
    binary_operation ( -. )
      (fun x y -> Lacaml.D.Vec.sub x y)
      (fun x y -> Lacaml.D.Mat.sub x y)
      ty x y

  let mul ty x y =
    binary_operation ( *. )
      (fun x y -> Lacaml.D.Vec.mul x y)
      (fun x y -> Lacaml.D.Mat.mul x y)
      ty x y

  let div ty x y =
    binary_operation ( /. )
      (fun x y -> Lacaml.D.Vec.div x y)
      (fun x y -> Lacaml.D.Mat.div x y)
      ty x y
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

  let zero n = Ops.zero (ty n) (get_v n)

  let get_d = function
    | C _ as c -> zero c
    | V u -> u.d

  let update_d node x = match node with
    | C _ -> ()
    | V v -> v.d <- Ops.add v.ty v.d x

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

  let mk_var ty v =
    V { ty ; v ; d = Ops.zero ty v }

  let unary_operation_gen f df xf stack =
    let x = xf stack in
    let ty = ty x in
    let z = mk_var ty (f ty (get_v x)) in
    let backward () =
      update_d x (df ty (get_d z) (get_v x))
    in
    Stack.push backward stack ;
    z

  let unary_operation f df =
    unary_operation_gen f (fun ty dz x ->
        Ops.mul ty dz (df ty x)
      )

  let neg x =
    unary_operation_gen
      Ops.neg
      (fun ty dz _ -> Ops.neg ty dz)
      x

  let inv x =
    unary_operation
      Ops.inv
      Ops.(fun ty x -> neg ty (inv ty (mul ty x x)))
      x

  let exp x =
    unary_operation Ops.exp Ops.exp x

  let binary_operation_gen f df1 df2 xf yf stack =
    let x = xf stack in
    let y = yf stack in
    let ty = ty x in
    let z = mk_var ty (f ty (get_v x) (get_v y)) in
    let backward () =
      update_d x (df1 ty (get_d z) (get_v x) (get_v y)) ;
      update_d y (df2 ty (get_d z) (get_v x) (get_v y))
    in
    Stack.push backward stack ;
    z

  let binary_operation f df1 df2 =
    binary_operation_gen f
      (fun ty dz x y -> Ops.mul ty dz (df1 ty x y))
      (fun ty dz x y -> Ops.mul ty dz (df2 ty x y))

  let add x y =
    binary_operation_gen
      Ops.add
      (fun _ dz _ _ -> dz)
      (fun _ dz _ _ -> dz)
      x y

  let sub x y =
    binary_operation_gen
      Ops.sub
      (fun  _ dz _ _ -> dz)
      (fun ty dz _ _ -> Ops.neg ty dz)
      x y

  let mul x y =
    binary_operation
      Ops.mul
      (fun _ _ y -> y)
      (fun _ x _ -> x)
      x y

  let div x y =
    binary_operation
      Ops.div
      (fun ty _ y -> Ops.inv ty y)
      (fun ty x y -> Ops.(div ty (neg ty x) (mul ty y y)))
      x y

  let let_in (xf : 'a exp) (f : 'a exp -> 'b exp) stack =
    let x = xf stack in
    f (Fun.const x) stack

  let (let*) = let_in

  let diag xf stack =
    let x_node = xf stack in
    let x = get_v x_node in
    let n = Vec.dim x in
    let m = Mat.init_cols n n (fun i j -> if i = j then x.{i} else 0.) in
    let z = mk_var Type.Matrix m in
    let backward () =
      let dz = get_d z in
      update_vec_d x_node (fun i -> dz.{i, i})
    in
    Stack.push backward stack ;
    z

  let vector_dot xf yf stack =
    let x = xf stack in
    let y = yf stack in
    let z = mk_var Type.Float (Lacaml.D.dot (get_v x) (get_v y)) in
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
    V { v = init ; ty ; d = Ops.zero ty init }

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
>>> w = torch.tensor([0.1,0.2,0,-0.1], requires_grad = True)
>>> b = torch.tensor(0.3, requires_grad = True)
>>> z = torch.sigmoid(torch.dot(w, w) + b)
>>> z.backward()
>>> w.grad
tensor([ 0.0484,  0.0968,  0.0000, -0.0484])
>>> b.grad
tensor(0.2421)
|} ;
  Diff.(
    let sigmoid x = div (float 1.) (add (float 1.) (exp (neg x))) in
    obs2
      (fun w b -> sigmoid (add (vector_dot w w) b))
      (Type.vector, Lacaml.D.Vec.of_array [|0.1;0.2;0.;-0.1|])
      (Type.float, 0.3)
  )
  |> [%show: float * (vector * float)]
  |> print_endline ;
  [%expect {|
    >>> w = torch.tensor([0.1,0.2,0,-0.1], requires_grad = True)
    >>> b = torch.tensor(0.3, requires_grad = True)
    >>> z = torch.sigmoid(torch.dot(w, w) + b)
    >>> z.backward()
    >>> w.grad
    tensor([ 0.0484,  0.0968,  0.0000, -0.0484])
    >>> b.grad
    tensor(0.2421)

    (0.589040434059, ( 0.0484144
                       0.0968287
                               0
                      -0.0484144, 0.242071801103)) |}]
