module Term = struct
  (** A de Bruijn index. *)
  type index = int

  (** A term. *)
  type t =
    | Var of index
    | Abs of t
    | App of t * t
    | Let of t * t

  let rec to_string = function
    | Var i -> string_of_int i
    | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
    | Abs t -> Printf.sprintf "λ %s" (to_string t)
    | Let (t, u) -> Printf.sprintf "let %s in\n%s" (to_string t) (to_string u)
end


(** A de Bruijn level. *)
type level = int

type environment = t list

(** A value. *)
and t =
  | Var of level
  | App of t * t
  | Abs of closure

and closure = environment * Term.t

(** Compute weak head normal form. *)
let rec eval (env : environment) : Term.t -> t = function
  | Var i -> List.nth env i
  | Abs t -> Abs (env, t)
  | App (t, u) ->
    let u = eval env u in
    (
      match eval env t with
      | Abs (env, t) -> eval (u::env) t
      | t -> App (t, u)
    )
  | Let (t, u) ->
    let t = eval env t in
    eval (t::env) u

(** Reify normal form. *)
let rec quote l : t -> Term.t = function
  | Var i -> Var (l-1 - i)
  | App (t, u) -> App (quote l t, quote l u)
  | Abs (env, t) ->
    let t = eval ((Var l)::env) t in
    Abs (quote (l+1) t)

(** Compute the normal form of a term. *)
let normalize env t =
  eval env t |> quote (List.length env)
