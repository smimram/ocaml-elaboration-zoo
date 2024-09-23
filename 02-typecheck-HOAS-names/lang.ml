module Term = struct
  (** A term. *)
  type t =
    | Var of string (** a variable *)
    | Abs of string * t (** abstraction *)
    | App of t * t
    | U
    | Pi of string * ty * ty (** Π-type *)
    | Let of string * ty * t * t

  (** A type (only for clarity, it's a term). *)
  and ty = t

  let rec abs l t =
    match l with
    | x::l -> Abs (x, abs l t)
    | [] -> t

  let rec to_string = function
    | Var x -> x
    | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (to_string t)
    | Let (x, t, a, u) -> Printf.sprintf "let %s : %s = %s;\n%s" x (to_string t) (to_string a) (to_string u)
    | U -> "𝒰"
    | Pi (x, a, t) -> Printf.sprintf "(%s : %s) → %s" x (to_string a) (to_string t)
end

(** A value. *)
type t =
  | Var of string
  | App of t * t
  | Abs of string * (t -> t)
  | Pi of string * ty * (t -> t)
  | U

(** A type. *)
and ty = t

(** An environment. *)
and environment = (string * t) list

(** String representation of a value. *)
let rec to_string = function
  | Var x -> x
  | App (t, u) -> Printf.sprintf "(%s %s)" (to_string t) (to_string u)
  | Abs (x, t) -> Printf.sprintf "λ%s.%s" x (to_string (t (Var x)))
  | Pi (x, a, t) -> Printf.sprintf "(%s : %s) → %s" x (to_string a) (to_string (t (Var x)))
  | U -> "𝒰"

(** Generate a fresh variable name looking like x. *)
let rec fresh env x =
  if x = "_" then "_" else
  if List.mem_assoc x env then fresh env (x^"'")
  else x

(** Compute the weak head normal form of a term (which is a value). *)
let rec eval (env : environment) : Term.t -> t = function
  | Var x -> List.assoc x env
  | Abs (x, t) -> Abs (x, fun v -> eval ((x,v)::env) t)
  | App (t, u) ->
    let u = eval env u in
    (
      match eval env t with
      | Abs (_, f) -> f u
      | t -> App (t, u)
    )
  | U -> U
  | Pi (x, a, b) -> Pi (x, eval env a, fun v -> eval ((x,v)::env) b)
  | Let (x, _a, t, u) ->
    let t = eval env t in
    eval ((x,t)::env) u

(** Reify a normal form. *)
let rec quote env : t -> Term.t = function
  | Var x -> Var x
  | App (t, u) -> App (quote env t, quote env u)
  | Abs (x, f) ->
    let x = fresh env x in
    Abs (x, quote ((x, Var x)::env) (f (Var x)))
  | Pi (x, a, b) ->
    let x = fresh env x in
    Pi (x, quote env a, quote ((x, Var x)::env) (b (Var x)))
  | U -> U

(** Compute the normal form of a term. *)
let normalize env t =
  eval env t |> quote env

(** Type-directed βη-conversion. *)
let rec conv env (t:t) (u:t) =
  (* Printf.printf "conv: %s = %s\n%!" (to_string t) (to_string u); *)
  match t, u with
  | U, U -> true
  | Pi (x, a, b), Pi (_, a', b') ->
    let x = fresh env x in
    conv env a a' && conv ((x,Var x)::env) (b (Var x)) (b' (Var x))
  | Abs (x, t), Abs (_, t') ->
    let x = fresh env x in
    conv ((x, Var x)::env) (t (Var x)) (t' (Var x))
  | Abs (x, t), u ->
    (* η-conversion *)
    conv ((x, Var x)::env) (t (Var x)) (App (u, Var x))
  | t, Abs (x, u) ->
    conv ((x, Var x)::env) (App (t, Var x)) (u (Var x))
  | Var x, Var x' -> x = x'
  | App (t, u), App (t', u') ->
    conv env t t' && conv env u u'
  | _ -> false

(** A typing error. *)
exception Typing of string

(** We are not smart enough to infer the type. *)
exception Inference

(** Check that a term has given type in given environments for terms and types. *)
let rec check env tenv (t : Term.t) (a : ty) =
  (* Printf.printf "check: %s : %s\n%!" (Term.to_string t) (to_string a); *)
  match t, a with
  | Abs (x, t), Pi (x', a, b) ->
    let x' = fresh env x' in
    check ((x, Var x')::env) ((x, a)::tenv) t (b (Var x'))
  | Let (x, a, t, u), b ->
    check env tenv a U;
    Printf.printf "** type of let of type U\n%!";
    let a = eval env a in
    check env tenv t a;
    Printf.printf "** let done\n%!";
    check ((x, eval env t)::env) ((x, a)::tenv) u b
  | _ ->
    (* The term cannot be checked, try to infer instead. *)
    let b = infer env tenv t in
    if not (conv env b a) then raise (Typing (Printf.sprintf "got %s but %s expected" (to_string b) (to_string a)))

(** Infer the type for a term. *)
and infer env tenv t : ty =
  (* Printf.printf "infer: %s\n%!" (Term.to_string t); *)
  match t with
  | Var x ->
    (
      match List.assoc_opt x tenv with
      | Some a -> a
      | None -> failwith ("Unbound variable " ^ x)
    )
  | U -> U (* type in type *)
  | App (t, u) ->
    let a = infer env tenv t in
    (
      match a with
      | Pi (_, a, b) ->
        check env tenv u a;
        b (eval env u)
      | _ -> raise (Typing (Printf.sprintf "got %s but Π-type expected" (to_string a)))
    )
  | Abs _ -> raise Inference
  | Pi (x, a, b) ->
    check env tenv a U;
    check ((x, Var x)::env) ((x, eval env a)::tenv) b U;
    U
  | Let (x, a, t, u) ->
    check env tenv a U;
    let a = eval env a in
    check env tenv t a;
    infer ((x, eval env t)::env) ((x, a)::tenv) u
