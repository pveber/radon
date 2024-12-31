let inv_log_2 = 1. /. Float.log 2.

module Vector = struct
  open Lacaml.D
  type t = vec

  let init size ~f = Vec.init size (fun i -> f (i - 1))

  let inplace_scale f a = scal f a

  let scale f a =
    let r = copy a in
    inplace_scale f r ;
    r

  let mul v1 v2 = Vec.mul v1 v2

  let dot v1 v2 = dot v1 v2
end

module Matrix = struct
  open Lacaml.D

  type t = mat

  let inplace_scal_mat_mul f a = Mat.scal f a

  let scale f a =
    let r = lacpy a in
    inplace_scal_mat_mul f r ;
    r

  let dotv m v = gemv m v

  let dot m n = gemm m n

  let norm1 x = lange ~norm:`O x

  let expm x =
    let m = Mat.dim1 x in
    let n = Mat.dim2 x in
    if m <> n then invalid_arg "matrix not square" ;
    (* trivial case *)
    if m = 1 && n = 1 then
      Mat.make 1 1 (Float.exp x.{1, 1})
    else (
      (* TODO: use gebal to balance to improve accuracy, refer to Julia's impl *)
      let xe = Mat.identity m in
      let norm_x = norm1 x in
      (* for small norm, use lower order Padé-approximation *)
      if norm_x <= 2.097847961257068 then (
        let c = (
          if norm_x > 0.9504178996162932 then
            [|17643225600.; 8821612800.; 2075673600.; 302702400.; 30270240.; 2162160.; 110880.; 3960.; 90.; 1.|]
          else if norm_x > 0.2539398330063230 then
            [|17297280.; 8648640.; 1995840.; 277200.; 25200.; 1512.; 56.; 1.|]
          else if norm_x > 0.01495585217958292 then
            [|30240.; 15120.; 3360.; 420.; 30.; 1.|]
          else
            [|120.; 60.; 12.; 1.|]
        ) in

        let x2 = gemm x x in
        let p = ref (lacpy xe) in
        let u = scale c.(1) !p in
        let v = scale c.(0) !p in

        for i = 1 to Array.(length c / 2 - 1) do
          let j = 2 * i in
          let k = j + 1 in
          p := gemm !p x2 ;
          Mat.axpy ~alpha:c.(k) !p u ;
          Mat.axpy ~alpha:c.(j) !p v ;
        done;

        let u = gemm x u in
        let a = Mat.sub v u in
        let b = Mat.add v u in
        gesv a b ;
        b
      )
      (* for larger norm, Padé-13 approximation *)
      else (
        let s = Float.log (norm_x /. 5.4) *. inv_log_2 in
        let t = ceil s in
        let x = if s > 0. then scale (2. ** (-. t)) x else x in

        let c =
          [|64764752532480000.; 32382376266240000.; 7771770303897600.;
            1187353796428800.;  129060195264000.;   10559470521600.;
            670442572800.;      33522128640.;       1323241920.;
            40840800.;          960960.;            16380.;
            182.;               1.|]
        in

        let x2 = gemm x x in
        let x4 = gemm x2 x2 in
        let x6 = gemm x2 x4 in
        let u =
          let m = lacpy x2 in
          inplace_scal_mat_mul c.(9) m ;
          Mat.axpy ~alpha:c.(11) x4 m ;
          Mat.axpy ~alpha:c.(13) x6 m ;
          let m = gemm x6 m in
          Mat.axpy ~alpha:c.(1) xe m ;
          Mat.axpy ~alpha:c.(3) x2 m ;
          Mat.axpy ~alpha:c.(5) x4 m ;
          Mat.axpy ~alpha:c.(7) x6 m ;
          gemm x m
        in
        let v =
          let m = lacpy x2 in
          inplace_scal_mat_mul c.(8) m ;
          Mat.axpy ~alpha:c.(10) x4 m ;
          Mat.axpy ~alpha:c.(12) x6 m ;
          let m = gemm x6 m in
          Mat.axpy ~alpha:c.(0) xe m ;
          Mat.axpy ~alpha:c.(2) x2 m ;
          Mat.axpy ~alpha:c.(4) x4 m ;
          Mat.axpy ~alpha:c.(6) x6 m ;
          m
        in
        let a = Mat.sub v u in
        let b = Mat.add v u in
        gesv a b ;

        let x = ref (lacpy b) in
        if s > 0. then (
          for _i = 1 to int_of_float t do
            x := gemm !x !x
          done;
        );
        !x
      )
    )
end

type vector = Vector.t
type matrix = Matrix.t

type _ ty =
  (* | TUnit : unit ty *)
  (* | TInt : int ty *)
  | TFloat : float ty
  | TVector : vector ty
  | TMatrix : matrix ty
  (* | TArrow : 'a ty * 'b ty -> ('a -> 'b) ty *)
  | TPair : 'a ty * 'b ty -> ('a * 'b) ty

let ty_fst
  : type a b. (a * b) ty -> a ty
  = function
    | TPair (u, _) -> u
    | TVector -> assert false
    | TMatrix -> assert false

let ty_snd
  : type a b. (a * b) ty -> b ty
  = function
    | TPair (_, v) -> v
    | TVector -> assert false
    | TMatrix -> assert false

let rec eval_zero
  : type a. a ty -> a -> a
  = fun ty x ->
    match ty with
    | TFloat -> 0.
    | TPair (a, b) -> (eval_zero a (fst x), eval_zero b (snd x))
    | TVector -> Lacaml.D.Vec.(make (dim x) 0.)
    | TMatrix -> Lacaml.D.Mat.(make (dim1 x) (dim2 x) 0.)
    (* | TUnit -> () *)
    (* | TInt -> 0 *)

let rec eval_add
  : type a. a ty -> a -> a -> a
  = fun ty x y ->
    match ty with
    | TFloat -> x +. y
    | TPair (ty_a, ty_b) ->
      (eval_add ty_a (fst x) (fst y),
       eval_add ty_b (snd x) (snd y))
    | TVector -> Lacaml.D.Vec.add x y
    | TMatrix -> Lacaml.D.Mat.add x y

let rec eval_mul
  : type a. a ty -> a -> a -> a
  = fun ty x y ->
    match ty with
    | TFloat -> x *. y
    | TPair (ty_a, ty_b) ->
      (eval_mul ty_a (fst x) (fst y),
       eval_mul ty_b (snd x) (snd y))
    | _ -> assert false

module type Category = sig
  type ('a, 'b) t
  val id : ('a, 'a) t
  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end

module type Monoidal = sig
  include Category
  (* val prod : ('a, 'c) t -> ('b, 'd) t -> ('a * 'b, 'c * 'd) t *)
end

module type Cartesian = sig
  include Monoidal
  val exl : ('a * 'b, 'a) t
  val exr : ('a * 'b, 'b) t
  val fork : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end

module type CCC = sig
  include Cartesian

  val apply : (('a -> 'b) * 'a, 'b) t
  val curry : ('a * 'b, 'c) t -> ('a, 'b -> 'c) t
  (* val uncurry : ('a, 'b -> 'c) t -> ('a * 'b, 'c) t *)

  val const : 'b ty -> 'b -> ('a, 'b) t

  type ('a, 'b) primitive
  val primitive : ('a, 'b) primitive -> ('a, 'b) t
end

module type Primitive = sig
  type (_, _) t
  val return_type : (_, 'a) t -> 'a ty
  (* val eval : ('a, 'b) t -> 'a -> 'b *)
end

module CCC(Primitive : Primitive) = struct
  type ('a, 'b) primitive = ('a, 'b) Primitive.t

  type (_, _) t =
    | Id : ('a, 'a) t
    | Compose : ('b, 'c) t * ('a, 'b) t -> ('a, 'c) t
    (* | Prod : ('a, 'c) t * ('b, 'd) t -> ('a * 'b, 'c * 'd) t *)
    | Exl : ('a * 'b, 'a) t
    | Exr : ('a * 'b, 'b) t
    | Fork : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
    | Const : 'b ty * 'b -> ('a, 'b) t
    | Apply : (('a -> 'b) * 'a, 'b) t
    | Curry : ('a * 'b, 'c) t -> ('a, 'b -> 'c) t
    | Uncurry : ('a, 'b -> 'c) t -> ('a * 'b, 'c) t
    | Primitive : ('a, 'b) Primitive.t -> ('a, 'b) t

  let id = Id
  let compose g f = Compose (g, f)
  let exl = Exl
  let exr = Exr
  let fork f g = Fork (f, g)
  let apply = Apply
  let curry f = Curry f
  (* let uncurry f = Uncurry f *)
  (* let prod x y = Prod (x, y) *)
  let primitive p = Primitive p
  let const ty c = Const (ty, c)

  (* let rec eval
   *   : type a b. (a, b) t -> a -> b
   *   = fun exp x ->
   *     match exp with
   *     | Id -> x
   *     | Compose (g, f) -> eval g (eval f x)
   *     | Prod (f, g) -> (eval f (fst x), eval g (snd x))
   *     | Exl -> fst x
   *     | Exr -> snd x
   *     | Fork (f, g) -> (eval f x, eval g x)
   *     | Apply -> (fst x) (snd x)
   *     | Curry f -> fun y -> eval f (x, y)
   *     | Uncurry f -> eval f (fst x) (snd x)
   *     | Primitive p -> Primitive.eval p x
   *     | Const (_, c) -> c *)

  let rec return_type
    : type u v. (u, v) t -> u ty -> v ty
    = fun ccc ty ->
      match ccc with
      | Id -> ty
      | Exl -> ty_fst ty
      | Exr -> ty_snd ty
      | Apply -> assert false
      | Curry _ -> assert false
      | Uncurry _ -> assert false
      | Compose (g, f) ->
        let f_out_ty = return_type f ty in
        return_type g f_out_ty
      (* | Prod (f, g) ->
       *   TPair (return_type f (ty_fst ty),
       *          return_type g (ty_snd ty)) *)
      | Fork (f, g) ->
        TPair (return_type f ty, return_type g ty)
      | Primitive p -> Primitive.return_type p
      | Const (ty, _) -> ty

  (* state monad *)
  module M = struct
    type 'a t = 'a * bool

    let return x = x, false
    let return_modified x = x, true
    let ( >>= ) (x, b1) f =
      let y, b2 = f x in
      y, (b1 || b2)
  end

  module Tactic = struct
    type ('a, 'b) term = ('a, 'b) t
    type t = {
      f : 'a 'b. ('a, 'b) term -> ('a, 'b) term M.t
    }

    let append t1 t2 = {
      f = fun x -> M.(t1.f x >>= t2.f)
    }

    let ( ++ ) = append

    let with_prior prior tactic = {
      f = fun term ->
        let (term', _) = prior.f term in
        tactic.f term'
    }

    let sequence xs = {
      f = fun x ->
        List.fold_left M.(fun acc f -> acc >>= f.f) (x, false) xs
    }

    let rec apply
      : type a b. t -> (a, b) term -> (a, b) term M.t
      = fun f term ->
        M.(f.f term >>= broadcast f)

    and broadcast
      : type a b. t -> (a, b) term -> (a, b) term M.t
      = fun tactic term ->
        let open M in
        match term with
        | Compose (a, b) ->
          apply tactic a >>= fun a ->
          apply tactic b >>= fun b ->
          return (Compose (a, b))
        | Curry a ->
          apply tactic a >>= fun a -> return (Curry a)
        | Uncurry a ->
          apply tactic a >>= fun a -> return (Uncurry a)
        | Fork (a, b) ->
          apply tactic a >>= fun a ->
          apply tactic b >>= fun b ->
          return (Fork (a, b))
        | term -> M.return term

    let repeated_apply t term =
      let rec loop (term, _) =
        let (term, b) as state' = apply t term in
        if b then loop state' else term
      in
      loop (term, false)
  end

  let universal_exponential_property
    : type a b. (a, b) t -> (a, b) t M.t
    = function
      | Compose (
          Apply,
          Fork (
            Compose (Curry f, Exl),
            Exr
          )
        ) -> M.return_modified f
      | term -> M.return term

  let apply_fork_curry
    : type a b. (a, b) t -> (a, b) t M.t
    = function
      | Compose (
          Apply,
          Fork (
            Compose (Curry f, g),
            h
          )
        ) -> M.return_modified (Compose (f, Fork (g, h)))
      | Compose (Apply,
                 Fork (Curry h, g)) ->
        M.return_modified (Compose (h, Fork (Id, g)))
      | term -> M.return term

  let compose_with_id
    : type a b. (a, b) t -> (a, b) t M.t
    = function
      | Compose (f, Id) -> M.return_modified f
      | Compose (Id, f) -> M.return_modified f
      | term -> M.return term

  let ex_fork
    : type a b. (a, b) t -> (a, b) t M.t
    = function
      | Compose (Exr, Fork (_, g)) -> M.return_modified g
      | Compose (Exl, Fork (f, _)) -> M.return_modified f
      | term -> M.return term

  let fork_ex
    : type a b. (a, b) t -> (a, b) t M.t
    = function
      | Fork (Exl, Exr) -> M.return_modified Id
      | term -> M.return term

  let assoc_right_compose
    : type a b. (a, b) t -> (a, b) t M.t
    = function
      | Compose (Compose (f, g), h) ->
        M.return_modified (Compose (f, Compose (g, h)))
      | term -> M.return term

  let assoc_right_compose = { Tactic.f = assoc_right_compose }

  let simplification_tactics = Tactic.[
    { f = universal_exponential_property } ;
    { f = apply_fork_curry } ;
    { f = compose_with_id } ;
    { f = ex_fork } ;
    { f = fork_ex } ;
  ]

  let simplification_tactic =
    let open Tactic in
    let simplifications = sequence simplification_tactics in
    simplifications
    ++ (with_prior assoc_right_compose simplifications)

  let simplify term =
    Tactic.repeated_apply simplification_tactic term
end

module Linear_map = struct
  type ('a, 'b) t = L of ('b -> 'a)

  let id = L (fun x -> x)
  let compose (L g) (L f) = L (fun v -> f (g v))
  (* let prod (L f) (L g) =
   *   L (fun (z, w) -> f z, g w) *)
  (* let dup ty = L (fun (du, dv) -> eval_add ty du dv) *)
  let exl zero = L (fun z -> z, zero)
  let exr zero = L (fun z -> zero, z)
  let fork ty (L f) (L g) =
    L (fun (du, dv) -> eval_add ty (f du) (g dv))
end

module Primitive = struct
  type (_, _) t =
    | Add : 'a ty -> ('a * 'a, 'a) t
    | Cos : (float, float) t
    | Diag : (vector, matrix) t
    | Exp : (float, float) t
    | Expm : (matrix, matrix) t
    | Inv : (float, float) t
    | Log : (float, float) t
    | Matrix_exp : (matrix, matrix) t
    | Matrix_dot : (matrix * matrix, matrix) t
    | Matrix_dotv : (matrix * vector, vector) t
    | Matrix_row_sums : (matrix, vector) t
    | Matrix_scale : (float * matrix, matrix) t
    | Matrix_sub : (matrix * matrix, matrix) t
    | Mul : (float * float, float) t
    | Sin : (float, float) t
    | Vector_dot : (vector * vector, float) t
    | Vector_exp : (vector, vector) t
    | Vector_mul : (vector * vector, vector) t
    | Vector_scale : (float * vector, vector) t
    | Vector_sum : (vector, float) t

  let return_type
    : type a b. (a, b) t -> b ty
    = function
      | Add ty -> ty
      | Cos -> TFloat
      | Exp -> TFloat
      | Log -> TFloat
      | Inv -> TFloat
      | Mul -> TFloat
      | Sin -> TFloat
      | Expm -> TMatrix
      | Matrix_dot -> TMatrix
      | Matrix_dotv -> TVector
      | Matrix_row_sums -> TVector
      | Matrix_scale -> TMatrix
      | Matrix_sub -> TMatrix
      | Vector_dot -> TFloat
      | Vector_exp -> TVector
      | Vector_mul -> TVector
      | Vector_scale -> TVector
      | Vector_sum -> TFloat
      | Diag -> TMatrix
      | Matrix_exp -> TMatrix

  let eval
    : type a b. (a, b) t -> a -> b
    = fun p x ->
      let open Lacaml.D in
      match p with
      | Add ty -> eval_add ty (fst x) (snd x)
      | Mul -> eval_mul TFloat (fst x) (snd x)
      | Cos -> Float.cos x
      | Sin -> Float.sin x
      | Log -> Float.log x
      | Exp -> Float.exp x
      | Inv -> 1. /. x
      | Expm -> Matrix.expm x
      | Matrix_dotv -> Matrix.dotv (fst x) (snd x)
      | Matrix_dot -> Matrix.dot (fst x) (snd x)
      | Matrix_row_sums ->
        Vec.init (Mat.dim1 x) (fun i ->
            let n = Mat.dim2 x in (* FIXME: suboptimal *)
            let sum = ref 0. in
            for j = 1 to n do
              sum := !sum +. x.{i, j}
            done ;
            !sum
          )
      | Matrix_scale -> Matrix.scale (fst x) (snd x)
      | Matrix_sub -> Mat.sub (fst x) (snd x)
      | Vector_dot -> Vector.dot (fst x) (snd x)
      | Vector_exp -> Vec.exp x
      | Vector_mul -> Vector.mul (fst x) (snd x)
      | Vector_scale -> Vector.scale (fst x) (snd x)
      | Vector_sum -> Vec.sum x
      | Diag ->
        let n = Vec.dim x in
        Mat.init_cols n n (fun i j -> if i = j then x.{i} else 0.)
      | Matrix_exp -> Mat.exp x

  let differential
    : type a b. (a, b) t -> a -> (b -> a)
    = fun p x ->
      let open Lacaml.D in
      match p with
      | Add _ ->
        fun dv -> dv, dv
      | Mul ->
        fun dv -> snd x *. dv, fst x *. dv
      | Cos ->
        fun dv -> Float.(-. sin x *. dv)
      | Sin ->
        fun dv -> Float.(cos x *. dv)
      | Exp ->
        fun dv -> Float.(exp x *. dv)
      | Log ->
        fun dv -> dv /. x
      | Expm ->
        fun dv ->
          let n = Mat.dim1 x in
          let bigmat =
            Mat.init_cols (2 * n) (2 * n) (fun i j ->
                match i <= n, j <= n with
                | true, true -> x.{j, i}
                | false, false -> x.{j - n, i - n}
                | true, false -> dv.{i, j - n}
                | false, true -> 0.
              ) in
          let exp_bigmat = Matrix.expm bigmat in
          Mat.init_cols n n (fun i j -> exp_bigmat.{i, j + n})
      | Matrix_dotv ->
        fun dv ->
          let open Lacaml.D in
          let m = fst x and x = snd x in
          let dp_dm =
            let r = Mat.make (Vec.dim x) (Vec.dim dv) 0. in
            ger dv x r ; r
          in
          let dp_dx = gemv ~trans:`T m dv in
          dp_dm, dp_dx
      | Vector_mul ->
        fun dv ->
          Vector.mul dv (snd x), Vector.mul dv (fst x)
      | Vector_dot ->
        fun dv ->
          Vector.scale dv (snd x), Vector.scale dv (fst x)
      | Matrix_scale ->
        fun dv ->
          Mat.sum_prod dv (snd x),
          Matrix.scale (fst x) dv
      | Vector_exp ->
        fun dv -> Vec.mul dv (Vec.exp x)
      | Vector_sum ->
        fun dv -> Vec.make (Vec.dim x) dv
      | Vector_scale ->
        fun dv ->
          dot dv (snd x), Vector.scale (fst x) dv
      | Inv ->
        fun dv -> -. dv /. (x ** 2.)
      | Matrix_dot ->
        fun dv ->
          gemm dv ~transb:`T (snd x),
          gemm ~transa:`T (fst x) dv
      | Matrix_sub ->
        fun dv -> dv, Matrix.scale (-1.) dv
      | Matrix_row_sums ->
        fun dv ->
          let r = Mat.make (Vec.dim dv) (Mat.dim2 x) 0. in
          ger dv (Vec.make (Mat.dim2 x) 1.) r ; r
      | Diag ->
        fun dv ->
          Vec.init (Vec.dim x) (fun i -> dv.{i, i})
      | Matrix_exp ->
        fun dv -> Mat.mul dv (Mat.exp x)
end

module D = struct
  type ('a, 'b) t = D of ('a -> ('b * ('a, 'b) Linear_map.t))

  let compose (D g) (D f) =
    D (
      fun x ->
        let y, f' = f x in
        let z, g' = g y in
        z, Linear_map.compose g' f'
    )

  (* let dup ty =
   *   D (
   *     fun x ->
   *       (x, x),
   *       Linear_map.dup ty
   *   ) *)

  (* let prod (D f) (D g) =
   *   D (
   *     fun (x, y) ->
   *       let f_x, f' = f x in
   *       let g_x, g' = g y in
   *       ((f_x, g_x), Linear_map.prod f' g')
   *   ) *)

  let exl ty = D (fun (x, y) -> (x, Linear_map.exl (eval_zero ty y)))
  let exr ty = D (fun (x, y) -> (y, Linear_map.exr (eval_zero ty x)))

  let fork ty (D f) (D g) = D (fun x ->
      let f_x, f' = f x in
      let g_x, g' = g x in
      (f_x, g_x), Linear_map.fork ty f' g')

  let primitive p =
    D (fun x -> Primitive.eval p x,
                Linear_map.L (Primitive.differential p x))
end

module STLC(CCC : CCC) = struct
  type ('a, 'b) refl = Refl : ('a, 'a) refl

  let rec eqtype
    : type u v. u ty -> v ty -> (u, v) refl option
    = fun t1 t2 ->
      match t1, t2 with
      (* | TUnit, TUnit -> Some Refl
       * | TUnit, _ -> None *)
      (* | TInt, TInt -> Some Refl
       * | TInt, _ -> None *)
      | TFloat, TFloat -> Some Refl
      | TFloat, _ -> None
      | TVector, TVector -> Some Refl
      | TVector, _ -> None
      | TMatrix, TMatrix -> Some Refl
      | TMatrix, _ -> None
      | TPair (fst1, snd1), TPair (fst2, snd2) -> (
          match eqtype fst1 fst2, eqtype snd1 snd2 with
          | Some Refl, Some Refl -> Some Refl
          | _ -> None
        )
      | TPair _, _ -> None
      (* | TArrow (u1, v1), TArrow (u2, v2) -> (
       *     match eqtype u1 u2, eqtype v1 v2 with
       *     | Some Refl, Some Refl -> Some Refl
       *     | _ -> None
       *   )
       * | TArrow _, _ -> None *)

  type _ t =
      | Var : var * 'a ty -> 'a t
      | Lam : var * 'a ty * 'b t -> ('a -> 'b) t
      | App : ('a -> 'b) t * 'a t -> 'b t
      | Fst : ('a * 'b -> 'a) t
      | Snd : ('a * 'b -> 'b) t
      | Pair : 'a t * 'b t -> ('a * 'b) t
      | Const : 'a ty * 'a -> 'a t
      | Primitive : ('a, 'b) CCC.primitive -> ('a -> 'b) t

    and var = string

    let var =
      let c = ref 0 in
      fun () ->
        incr c ;
        "x_" ^ Int.to_string !c

    let rec substitution
      : type u v. u t -> replace:(var * v ty) -> by:v t -> u t
      = fun term ~replace:((v_id, v_ty) as v) ~by:t ->
        match term with
        | Var (w_id, w_ty) as w ->
          if v_id = w_id then (
            match eqtype v_ty w_ty with
            | Some Refl -> t
            | None -> invalid_arg "invalid substitution"
          )
          else w
        | Lam (w_id, w_ty, body) ->
          if v_id = w_id then term
          else Lam (w_id, w_ty, substitution body ~replace:v ~by:t)
        | App (_U_, _V_) ->
          App (
            substitution _U_ ~replace:v ~by:t,
            substitution _V_ ~replace:v ~by:t
          )
        | Pair (_U_, _V_) ->
          Pair (
            substitution _U_ ~replace:v ~by:t,
            substitution _V_ ~replace:v ~by:t
          )
        | Fst -> Fst
        | Snd -> Snd
        | Const _ as c -> c
        | Primitive _ as p -> p

  let ( $ ) f x = App (f, x)
  let fst x = Fst $ x
  let snd y = Snd $ y
  let pair x y = Pair (x, y)

  let constfun f = CCC.(curry (compose f exr))

  let rec translate
    : type a b. (a -> b) t -> (a, b) CCC.t
    = function
      | Lam (x_id, x_ty, App (u, v)) ->
        CCC.(
          compose
            apply
            (fork
               (translate (Lam (x_id, x_ty, u)))
               (translate (Lam (x_id, x_ty, v))))
        )
      | Lam (x_id, x_ty, (Lam (y_id, y_ty, u))) ->
        let r_id = var () in
        let r_ty = TPair (x_ty, y_ty) in
        let r = Var (r_id, r_ty) in
        let body =
          substitution u ~replace:(x_id, x_ty) ~by:(App (Fst, r))
          |> substitution ~replace:(y_id, y_ty) ~by:(App (Snd, r))
        in
        CCC.curry (translate (Lam (r_id, r_ty, body)))
      | Lam (v_id, v_ty, Var (w_id, w_ty)) ->
        if v_id = w_id then (
          match eqtype v_ty w_ty with
          | Some Refl -> CCC.id
          | None ->
            let msg = Printf.sprintf "inconsistent type for %s" w_id in
            invalid_arg msg
        )
        else invalid_arg (Printf.sprintf "free variable %s" w_id)
      | Lam (x_id, x_ty, Pair (u, v)) ->
        CCC.(
          fork
            (translate (Lam (x_id, x_ty, u)))
            (translate (Lam (x_id, x_ty, v)))
        )
      | Lam (_, _, Fst) -> constfun CCC.exl
      | Fst -> CCC.exl
      | Lam (_, _, Snd) -> constfun CCC.exr
      | Snd -> CCC.exr
      | Lam (_, _, Primitive p) -> constfun (CCC.primitive p)
      | Lam (_, _, Const (ty, c)) -> CCC.const ty c
      | Const _ -> assert false
      | Primitive p -> CCC.(primitive p)
      | Var (v_id,_) ->
        invalid_arg (Printf.sprintf "free variable %s" v_id)
      | App (_, _) -> invalid_arg "app not supported"
end

module DSL = struct
  type nonrec 'a ty = 'a ty
  module CCC = CCC(Primitive)
  include STLC(CCC)

  type 'a exp = 'a t
  let tfloat = TFloat
  let tvector = TVector
  let tmatrix = TMatrix
  let tpair t1 t2 = TPair (t1, t2)

  open Primitive

  let const ty c = Const (ty, c)
  let float x = const tfloat x
  let vector v = const tvector v
  let matrix m = const tmatrix m

  let add ty x y = Primitive (Add ty) $ Pair (x, y)
  let diag x = Primitive Diag $ x
  let mul x y = Primitive Mul $ Pair (x, y)
  let cos x = Primitive Cos $ x
  let sin x = Primitive Sin $ x
  let inv x = Primitive Inv $ x
  let log x = Primitive Log $ x
  let exp x = Primitive Exp $ x
  let matrix_expm x = Primitive Expm $ x
  let matrix_exp x = Primitive Matrix_exp $ x
  let matrix_dotv x y = Primitive Matrix_dotv $ Pair (x, y)
  let matrix_dot x y = Primitive Matrix_dot $ Pair (x, y)
  let matrix_row_sums m = Primitive Matrix_row_sums $ m
  let matrix_sub x y = Primitive Matrix_sub $ Pair (x, y)
  let matrix_scale lambda m = Primitive Matrix_scale $ Pair (lambda, m)
  let vector_dot x y = Primitive Vector_dot $ Pair (x, y)
  let vector_exp x = Primitive Vector_exp $ x
  let vector_mul x y = Primitive Vector_mul $ Pair (x, y)
  let vector_scale lambda u = Primitive Vector_scale $ Pair (lambda, u)
  let vector_sum x = Primitive Vector_sum $ x

  let rec d_of_ccc
    : type a b. a ty -> (a, b) CCC.t -> (a, b) D.t
    = fun ty ccc ->
      match ccc with
      | Id -> D (fun x -> x, Linear_map.id)
      | Exl -> D.exl (ty_snd ty)
      | Exr -> D.exr (ty_fst ty)
      | Apply -> assert false
      | Curry _ -> assert false
      | Uncurry _ -> assert false
      | Compose (g, f) ->
        let f_out_ty = CCC.return_type f ty in
        D.compose (d_of_ccc f_out_ty g) (d_of_ccc ty f)
      (* | Prod (f, g) -> D.prod (d_of_ccc (ty_fst ty) f) (d_of_ccc (ty_snd ty) g) *)
      | Fork (f, g) -> D.fork ty (d_of_ccc ty f) (d_of_ccc ty g)
      | Primitive p -> D.primitive p
      | Const (_, c) -> D (fun x -> c, let zero = eval_zero ty x in L (fun _ -> zero))

  let ccc_of_lambda f ty =
    let x_id = var () in
    Lam (x_id, ty, f (Var (x_id, ty)))
    |> translate
    |> CCC.simplify

  let eval f ty =
    let D f = d_of_ccc ty (ccc_of_lambda f ty) in
    fun x ->
      let y, _ = f x in
      y

  let grad f ty =
    let D f = d_of_ccc ty (ccc_of_lambda f ty) in
    fun x ->
      let y, Linear_map.L l = f x in
      y, l 1.

end
