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

(** String representation of a term, in a context whose variables are named
    vars (the most recent first). When pa is true, the result is parenthesized
    unless it is atomic. *)
let rec to_string ?(pa=false) vars t =
  let pa s = if pa then "(" ^ s ^ ")" else s in
  let var n = if n < 0 || n >= List.length vars then Printf.sprintf "x#%d" n else List.nth vars n in
  match t with
  | Let (x,a,t,u) ->
    pa @@ Printf.sprintf "let %s : %s = %s;\n%s" x (to_string vars a) (to_string vars t) (to_string (x::vars) u)
  | Abs ((x,i),t) ->
    let x' = match i with `Explicit -> x | `Implicit -> "{" ^ x ^ "}" in
    pa @@ Printf.sprintf "λ%s. %s" x' (to_string (x::vars) t)
  | App (t,(i,u)) ->
    let t = match t with App _ -> to_string vars t | _ -> to_string ~pa:true vars t in
    let u = match i with `Explicit -> to_string ~pa:true vars u | `Implicit -> "{" ^ to_string vars u ^ "}" in
    pa @@ Printf.sprintf "%s %s" t u
  | Var n -> var n
  | Pi (("_",`Explicit,a),b) ->
    let a = match a with Pi _ | Abs _ | Let _ -> to_string ~pa:true vars a | _ -> to_string vars a in
    pa @@ Printf.sprintf "%s → %s" a (to_string ("_"::vars) b)
  | Pi ((x,i,a),b) ->
    pa @@ Printf.sprintf "%s → %s" (icit_pa i (x ^ " : " ^ to_string vars a)) (to_string (x::vars) b)
  | Type -> "U"
  | Meta m -> "?" ^ string_of_int m
  | InsertedMeta (m,bds) ->
    (* The metavariable is applied to the bound variables. *)
    let args = List.filteri (fun n _ -> List.nth bds n = `Bound) (List.init (List.length bds) var) in
    if args = [] then "?" ^ string_of_int m
    else pa @@ String.concat " " (("?" ^ string_of_int m) :: List.rev args)

let to_string ?(vars=[]) t = to_string vars t

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
