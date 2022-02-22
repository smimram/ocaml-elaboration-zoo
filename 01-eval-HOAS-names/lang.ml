module Term = struct
  (** A term. *)
  type t =
    | Var of string
    | Abs of string * t
    | App of t * t
    | Let of string * t * t

  let rec abs xx t =
    match xx with
    | [x] -> Abs (x, t)
    | x::xx -> Abs(x, abs xx t)
    | [] -> assert false

  let rec to_string = function
    | Var x -> x
    | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
    | Abs (x, t) -> Printf.sprintf "λ%s.%s" x (to_string t)
    | Let (x, t, u) -> Printf.sprintf "let %s = %s in\n%s" x (to_string t) (to_string u)
end

type environment = (string * t) list

(** A value. *)
and t =
  | Var of string
  | App of t * t
  | Abs of string * (t -> t)

and closure = environment * string * Term.t

(** Compute weak head normal form. *)
let rec eval (env : environment) = function
  | Term.Var x ->
    Option.value ~default:(Var x) (List.assoc_opt x env)
  | Abs (x, t) -> Abs (x, fun u -> eval ((x,u)::env) t)
  | App (t, u) ->
    let u = eval env u in
    (
      match eval env t with
      | Abs (_, t) -> t u
      | t -> App (t, u)
    )
  | Let (x, t, u) ->
    let t = eval env t in
    eval ((x,t)::env) u

(** Create a fresh variable name among ns based on x. *)
let rec fresh ns x =
  if x = "_" then "_"
  else if List.mem x ns then fresh ns (x^"'")
  else x

(** Reify normal form. *)
let rec quote ns = function
  | Var x -> Term.Var x
  | App (t, u) -> Term.App (quote ns t, quote ns u)
  | Abs (x, t) ->
    let x' = fresh ns x in
    let t = t (Var x') in
    Term.Abs (x', quote (x'::ns)  t)

(** Compute the normal form of a term. *)
let normalize env t =
  eval env t |> quote (List.map fst env)
  
