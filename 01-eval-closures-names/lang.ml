module Term = struct
  (** A term. *)
  type t =
    | Var of string
    | Abs of string * t
    | App of t * t
    | Let of string * t * t

  (** Abstract over multiple variables. *)
  let rec abs xx t =
    match xx with
    | [x] -> Abs (x, t)
    | x::xx -> Abs(x, abs xx t)
    | [] -> assert false

  (** String representation. *)
  let rec to_string = function
    | Var x -> x
    | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (to_string t)
    | Let (x, t, u) -> Printf.sprintf "let %s = %s in\n%s" x (to_string t) (to_string u)
end

(** A value. *)
type t =
  | Abs of environment * string * Term.t  (** A λ-abstraction in an environment. *)
  | Neu of neutral

(** A neutral value. *)
and neutral =
  | Var of string
  | App of neutral * t

and environment = (string * t) list

(** Compute weak head normal form. *)
let rec eval (env : environment) : Term.t -> t = function
  | Var x -> Option.value ~default:(Neu (Var x)) (List.assoc_opt x env)
  | Abs (x, t) -> Abs (env, x, t)
  | App (t, u) ->
    let u = eval env u in
    (
      match eval env t with
      | Abs (env, x, t) -> eval ((x,u)::env) t
      | Neu t -> Neu (App (t, u))
    )
  | Let (x, t, u) ->
    let t = eval env t in
    eval ((x,t)::env) u

(** Create a natural fresh variable name among ns based on x. *)
let rec fresh ns x =
  if x = "_" then "_"
  else if List.mem x ns then fresh ns (x^"'")
  else x

(** Reify normal form. *)
let rec quote ns : t -> Term.t = function
  | Abs (env, x, t) ->
    let x' = fresh ns x in
    let t = eval ((x, Neu (Var x'))::env) t in
    Abs (x', quote (x'::ns) t)
  | Neu t ->
    match t with
    | Var x -> Var x
    | App (t, u) -> App (quote ns (Neu t), quote ns u)

(** Compute the normal form of a term. *)
let normalize env t =
  eval env t |> quote (List.map fst env)
