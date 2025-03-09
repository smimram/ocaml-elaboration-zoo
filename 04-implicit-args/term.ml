(** Terms are unevaluated expressions. Compared to pre-terms, variables are in de Bruijn indices. *)

open Common

(** An expression. *)
type t =
  | Let of (string * ty * t * t)
  | Abs of (string * icit) * t (** λ-abstraction *)
  | App of t * (icit * t)
  | Var of int
  | Pi of (string * icit * ty) * t
  | Meta of meta
  | InsertedMeta of meta * [`Bound | `Defined] list
  | Type (** the type of types *)

and ty = t

and meta = int

let rec to_string vars = function
  | Let (x,a,t,u) ->
    Printf.sprintf "let %s : %s = %s in\n%s" x (to_string vars a) (to_string vars t) (to_string vars u)
  | Abs ((x,i),t) ->
    let x = icit_pa i x in
    Printf.sprintf "fun %s -> %s" x (to_string (x::vars) t)
  | App (t,(i,u)) ->
    Printf.sprintf "%s %s" (to_string vars t) (icit_pa i (to_string vars u))
  | Var n -> if n < 0 || n >= List.length vars then Printf.sprintf "x#%d" n else List.nth vars n
  | Pi ((x,i,a),b) ->
    let x = icit_pa i (x ^ " : " ^ to_string vars a) in
    Printf.sprintf "%s -> %s" x (to_string vars b)
  | Type -> "type"
  | Meta m -> "?" ^ string_of_int m
  | InsertedMeta (m,_) -> "?" ^ string_of_int m

let to_string ?(vars=[]) = to_string vars

let abss xx t =
  let rec aux xx =
    match xx with
    | x::xx -> Abs (x, aux xx)
    | [] -> t
  in
  aux xx

let rec rev_apps_explicit t = function
  | u::uu -> App (rev_apps_explicit t uu, (`Explicit, u))
  | [] -> t
