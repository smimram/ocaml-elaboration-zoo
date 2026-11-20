module RawTerm = struct
  (** A raw term (this is what we get out of the parser). *)
  type t =
    | Var of string
    | Abs of string * t
    | App of t * t
    | U
    | Pi of string * ty * ty
    | Let of string * ty * t * t

  (** A type (only for clarity, it's a term). *)
  and ty = t

  (** String representation. *)
  let rec to_string = function
    | Var x -> x
    | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (to_string t)
    | Let (x, t, a, u) -> Printf.sprintf "let %s : %s = %s in\n%s" x (to_string t) (to_string a) (to_string u)
    | U -> "𝒰"
    | Pi ("_", a, t) -> Printf.sprintf "(%s → %s)" (to_string a) (to_string t)
    | Pi (x, a, t) -> Printf.sprintf "((%s : %s) → %s)" x (to_string a) (to_string t)

  (** Abstract over multiple variables. *)
  let rec abs xx t =
    match xx with
    | [x] -> Abs (x, t)
    | x::xx -> Abs(x, abs xx t)
    | [] -> assert false
end

module Term = struct
  (** A term (a raw term massaged a bit to have de Bruijn indices and metavariables). *)
  type t =
    | Var of int (** a variable with given de Bruijn index *)
    | Abs of string * t (** abstraction (the variable name is only used for printing) *)
    | App of t * t
    | U
    | Pi of string * ty * ty (** Π-type (the variable name is only used for printing) *)
    | Let of string * ty * t * t

  (** A type (only for clarity, it's a term). *)
  and ty = t

  let rec to_string = function
    | Var i -> string_of_int i
    | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (to_string t)
    | Let (x, t, a, u) -> Printf.sprintf "let %s : %s = %s;\n%s" x (to_string t) (to_string a) (to_string u)
    | U -> "𝒰"
    | Pi ("_", a, t) ->
      let a = match a with Pi _ -> "(" ^ to_string a ^ ")" | _ -> to_string a in
      Printf.sprintf "%s → %s" a (to_string t)
    | Pi (x, a, t) -> Printf.sprintf "(%s : %s) → %s" x (to_string a) (to_string t)
end

(** A value. *)
type t =
  | Var of int
  | App of t * t
  | Abs of environment * string * Term.t
  | Pi of environment * string * ty * Term.t
  | U

(** A type. *)
and ty = t

(** An environment. *)
and environment = t list

(** String representation of a value. *)
let rec to_string = function
  | Var i -> string_of_int i
  | App (t, u) -> Printf.sprintf "(%s %s)" (to_string t) (to_string u)
  | Abs (_, x, t) -> Printf.sprintf "λ%s.%s" x (Term.to_string t)
  | Pi (_, x, a, t) -> Printf.sprintf "(%s : %s) → %s" x (to_string a) (Term.to_string t)
  | U -> "𝒰"

(** Compute the weak head normal form of a term (which is a value). *)
let rec eval (env : environment) : Term.t -> t = function
  | Var i -> List.nth env i
  | Abs (x, t) -> Abs (env, x, t)
  | App (t, u) ->
    let u = eval env u in
    (
      match eval env t with
      | Abs (env, _, t) -> eval (u::env) t
      | t -> App (t, u)
    )
  | U -> U
  | Pi (x, a, b) -> Pi (env, x, eval env a, b)
  | Let (_, _, t, u) ->
    let t = eval env t in
    eval (t::env) u

(** Reify a normal form. *)
let rec quote l : t -> Term.t = function
  | Var i -> Var (l-1 - i)
  | App (t, u) -> App (quote l t, quote l u)
  | Abs (env, x, t) ->
    let t = eval ((Var l)::env) t in
    Abs (x, quote (l+1) t)
  | Pi (env, x, a, b) ->
    let b = eval ((Var l)::env) b in
    Pi (x, quote l a, quote (l+1) b)
  | U -> U

(** Compute the normal form of a term. *)
let normalize env t =
  eval env t |> quote (List.length env)

(** Type-directed βη-conversion. *)
let rec conv l t u =
  (* Printf.printf "conv: %s = %s\n%!" (to_string t) (to_string u); *)
  match t, u with
  | U, U -> true
  | Pi (env, _, a, b), Pi (env', _, a', b') ->
    let b = eval ((Var l)::env) b in
    let b' = eval ((Var l)::env') b' in
    conv l a a' && conv (l+1) b b'
  | Abs (env, _, t), Abs (env', _, t') ->
    let t = eval ((Var l)::env) t in
    let t' = eval ((Var l)::env') t' in
    conv (l+1) t t'
  | Abs (env, _, t), u ->
    let t = eval ((Var l)::env) t in
    let u = App (u, Var l) in
    conv (l+1) t u
  | t, Abs (env, _, u) ->
    let t = App (t, Var l) in
    let u = eval ((Var l)::env) u in
    conv (l+1) t u
  | Var i, Var i' -> i = i'
  | App (t, u), App (t', u') ->
    conv l t t' && conv l u u'
  | _ -> false

(** A typing error. *)
exception Typing of string

let typing_error fmt = Printf.ksprintf (fun s -> raise (Typing s)) fmt

(** Check that a term has given type in given environments for terms and
    types. In passing, we translate the term in raw presentation into a term in
    de Bruijn presentation. *)
let rec check env tenv l (t : RawTerm.t) (a : ty) : Term.t =
  (* Printf.printf "check: %s : %s\n%!" (RawTerm.to_string t) (to_string a); *)
  match t, a with
  | Abs (x, t), Pi (env', _, a, b) ->
    let b = eval ((Var l)::env') b in
    let t = check ((Var l)::env) ((x,a)::tenv) (l+1) t b in
    Abs (x, t)
  | Let (x, a, t, u), b ->
    let a = check env tenv l a U in
    let va = eval env a in
    let t = check env tenv l t va in
    let vt = eval env t in
    let u = check (vt::env) ((x,va)::tenv) (l+1) u b in
    Let (x, a, t, u)
  | _ ->
    (* The term cannot be checked, try to infer instead. *)
    let t, b = infer env tenv l t in
    if not (conv l b a) then typing_error "got %s but %s expected" (Term.to_string (quote l b)) (Term.to_string (quote l a));
    t

(** Infer the type for a term and generate the term associated to the raw term along the way. *)
and infer env tenv l (t : RawTerm.t) : Term.t * ty =
  (* Printf.printf "infer: %s\n%!" (RawTerm.to_string t); *)
  match t with
  | Var x ->
    let rec aux i = function
      | (x',a)::l -> if x' = x then Term.Var i, a else aux (i+1) l
      | [] -> typing_error "unbound variable %s" x
    in
    aux 0 tenv
  | U -> U, U (* type in type *)
  | App (t, u) ->
    let t, a = infer env tenv l t in
    (
      match a with
      | Pi (env', _, a, b) ->
        let u = check env tenv l u a in
        let vu = eval env u in
        App (t, u), eval (vu::env') b
      | a -> typing_error "got %s but Π-type expected" (Term.to_string (quote l a))
    )
  | Abs _ -> typing_error "cannot infer the type of an abstraction"
  | Pi (x, a, b) ->
    let a = check env tenv l a U in
    let va = eval env a in
    let b = check ((Var l)::env) ((x,va)::tenv) (l+1) b U in
    Pi (x, a, b), U
  | Let (x, a, t, u) ->
    let a = check env tenv l a U in
    let va = eval env a in
    let t = check env tenv l t va in
    let vt = eval env t in
    let u, b = infer (vt::env) ((x,va)::tenv) (l+1) u in
    Let (x, a, t, u), b
