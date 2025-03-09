open Common
open Extlib

type level = int

type term = Term.t

type t =
  | Abs of (string * icit) * closure
  | Var of level * spine (* a variable applied to arguments *)
  | Meta of meta * spine
  | Pi of (string * icit * ty) * closure
  | Type

  | Unit | U
  | Nat | Z | S of t option | Ind_nat of t list

and ty = t

and environment = t list

(** A list of arguments. Important note this is reversed compared to the natural order: the first element of the list is the outermost argument! *)
and spine = (icit * t) list

and closure = environment * term

and meta =
  {
    id : int;
    mutable value : t option;
  }

type value = t

(*
(** Simple string representation. For debugging purposes only, otherwise use [to_string] below. *)
let rec to_string_simple ?(pa=false) t =
  let pa s = if pa then "(" ^ s ^ ")" else s in
  let icit i t = match i with `Implicit -> "{" ^ to_string_simple t ^ "}" | `Explicit -> to_string_simple ~pa:true t in
  let spine s = if s = [] then "" else " " ^ String.concat " " @@ List.rev_map (fun (i,t) -> icit i t) s in
  match t with
  | Abs ((x,_i),(_env,_t)) -> pa @@ Printf.sprintf "fun (%s) -> _" x
  | Var (x,_i) -> "x" ^ string_of_int x
  | Meta (m,s) ->
    let m =
      match m.value with
      | Some t -> to_string_simple ~pa:true t
      | None -> "?" ^ string_of_int m.id
    in
    m ^ spine s
  | Pi ((x,_,a),_) -> pa @@ Printf.sprintf "(%s : %s) => _" x (to_string_simple a)
  | Type -> "type"
  | Unit -> "unit"
  | U -> "()"
  | Nat -> "nat"
  | Ind_nat s -> pa ("ind_nat" ^ spine (List.map (fun t -> `Explicit, t) s))
  | Z -> "Z"
  | S None -> "S"
  | S (Some t) -> pa ("S " ^ to_string_simple ~pa:true t)
*)

let metavariables = Dynarray.create ()

(** Generate a fresh metavariable. *)
let fresh_meta () =
  let m = { id = Dynarray.length metavariables; value = None } in
  Dynarray.add_last metavariables m;
  m

(** Get metavariable with given id. *)
let get_meta id =
  Dynarray.get metavariables id

module IntMap = Map.Make(Int)

(** A partial renaming from Γ to Δ. *)
type partial_renaming =
  {
    dom : int; (** length of Γ *)
    cod : int; (** length of Δ *)
    ren : int IntMap.t; (** map the variables of Δ to those of Γ *)
  }

(** Create a variable. *)
let var x = Var (x, [])

(** Create a (non-dependent) arrow. *)
let arr a b = Pi (("_", `Explicit, a), ([], b))

(** Generate a fresh variable name. *)
let fresh_var_name =
  let h = Hashtbl.create 100 in
  fun x ->
    let n = Option.value ~default:0 @@ Hashtbl.find_opt h x in
    Hashtbl.replace h x (n+1);
    x ^ "#" ^ string_of_int n

(** Evaluate a term to a value. *)
let rec eval (env:environment) (t:term) =
  match t with
  | Let (_,_,t,u) ->
    let t = eval env t in
    eval (t::env) u
  | Abs ((x,i),u) ->
    Abs ((x,i),(env,u))
  | App (t,(i,u)) ->
    let t = eval env t in
    let u = eval env u in
    app t (i,u)
  | Var x ->
    List.nth env x
  | Pi ((x,i,a),b) ->
    let a = eval env a in
    Pi ((x,i,a),(env,b))
  | Meta m -> Meta (get_meta m, [])
  | InsertedMeta (m, bds) ->
    let m = get_meta m in
    let t =
      match m.value with
      | Some t -> t
      | None -> Meta (m, [])
    in
    let s = List.filter_map2 (fun t d -> if d = `Bound then Some (`Explicit, t) else None) env bds in
    app_spine t s
  | Type -> Type
  | Unit -> Unit
  | U -> U
  | Nat -> Nat
  | Z -> Z
  | S -> S None
  | Ind_nat -> Ind_nat []

(** Apply a value to another. *)
and app (t:t) (i,u) =
  match t with
  | Abs ((_,i'), (env,t)) -> assert (i = i'); eval (u::env) t
  | Var (x,s) -> Var (x, (i,u)::s)
  | Meta (m,s) -> Meta (m,(i,u)::s)
  | S None -> S (Some u)
  | _ -> failwith "TODO: unhandled app"

(** Apply a value to a spine. *)
and app_spine (t:value) = function
  | u::s -> app (app_spine t s) u
  | [] -> t

(** Replace metavariables by their value. *)
let rec force = function
  | Meta ({ value = Some t; _ }, s) ->
    force (app_spine t s)
  | t -> t

(** Reify a value as a term. *)
let rec quote l (t:t) : term =
  let rec app_spine t : spine -> term = function
    | (i,u)::s -> App (app_spine t s, (i, quote l u))
    | [] -> t
  in
  let rec app_explicit_spine t : t list -> term = function
    | u::s -> App (app_explicit_spine t s, (`Explicit, quote l u))
    | [] -> t
  in
  match force t with
  | Abs ((x,i),(env,t)) ->
    let t = quote (l+1) @@ eval ((var l)::env) t in
    Abs ((x,i),t)
  | Var (x,s) ->
    app_spine (Var (l-1-x)) s
  | Pi ((x,i,a),(env,b)) ->
    let a = quote l a in
    let b = quote (l+1) @@ eval ((var l)::env) b in
    Pi ((x,i,a),b)
  | Meta (m, s) ->
    app_spine (Meta m.id) s
  | Type -> Type
  | Unit -> Unit
  | U -> U
  | Nat -> Nat
  | Z -> Z
  | S None -> S
  | S (Some t) -> App (S, (`Explicit, quote l t))
  | Ind_nat s -> app_explicit_spine Ind_nat s

let normalize env t = quote 0 @@ eval env t

let to_string ?(vars=[]) t = Term.to_string ~vars @@ quote 0 t

let string_of_meta vars m =
  let m = get_meta m in
  match m.value with
  | Some t -> to_string ~vars t
  | None -> "?" ^ string_of_int m.id

exception Unification

(** Unify two values. *)
let rec unify l (t:t) (u:t) =
  match force t, force u with
  | Abs ((_,i),(env,b)), Abs ((_,i'),(env',b')) ->
    unify_check (i = i');
    let b = eval ((var l)::env) b in
    let b' = eval ((var l)::env') b' in
    unify (l+1) b b'
  | Pi ((_,i,a),(env,b)), Pi ((_,i',a'),(env',b')) ->
    unify_check (i = i');
    unify l a a';
    let b = eval ((var l)::env) b in
    let b' = eval ((var l)::env') b' in
    unify (l+1) b b'
  | Type, Type -> ()
  | Unit, Unit -> ()
  | Nat, Nat -> ()
  | Z, Z -> ()
  | S t, S t' ->
    (
      match t, t' with
      | None, None -> ()
      | Some t, Some t' -> unify l t t'
      | _ -> raise Unification
    )
  | Meta (m,s), Meta (m',s') when m.id = m'.id -> unify_spines l s s'
  | Meta (m,s), t -> unify_solve l m s t
  | t, Meta (m,s) -> unify_solve l m s t
  | _ -> raise Unification

and unify_check b = if not b then raise Unification

and unify_spines l s s' =
  unify_check (List.length s = List.length s');
  List.iter2 (fun (i,t) (i',t') -> unify_check (i = i'); unify l t t') s s'

(** Given a context Γ, make sure that a meta-variable ?α applied to the spine s equals to a term t. *)
and unify_solve l m s t =
  (* Printf.printf "***solve ?%d\n" m.id; *)
  (* From Γ and the spine, we construct a partial renaming from Γ to Δ (ie a partial function from the variables of Δ to those of Γ *)
  let pren =
    let rec aux = function
      | (_,t)::s ->
        let dom, ren = aux s in
        (
          match force t with
          | Var (x, []) when not (IntMap.mem x ren) -> dom+1, IntMap.add x dom ren
          | _ -> raise Unification
        )
      | [] -> 0, IntMap.empty
    in
    let dom, ren = aux s in
    { dom; cod = l; ren }
  in
  (* Add an extra bound variable to a renaming, ie we go from σ : Γ → Δ to Γ,x:A[σ] → Δ,x:A. *)
  let lift pren =
    { dom = pren.dom+1; cod = pren.cod+1; ren = IntMap.add pren.cod pren.dom pren.ren }
  in
  (* Apply a partial renaming to a value. Along the way, we also make sure that the metavariable does not occur in the term (occurs check). *)
  let rename m pren (t : value) =
    let rec aux pren : value -> term = function
      | Meta (m',s) ->
        unify_check (m <> m'); (* occurs check *)
        aux_spine pren (Meta m'.id) s
      | Abs ((x,i),(env,t)) ->
        let t = eval ((var pren.cod)::env) t in
        Abs ((x,i),aux (lift pren) t)
      | Var (n, s) ->
        (
          match IntMap.find_opt n pren.ren with
          | Some n' -> aux_spine pren (Var (pren.dom-1-n')) s
          | None -> raise Unification (* we have an escaping variable *)
        )
      | Pi ((x,i,a),(env,t)) ->
        let t = eval ((var pren.cod)::env) t in
        Pi ((x,i,aux pren a), aux (lift pren) t)
      | Type -> Type
      | Unit -> Unit
      | U -> U
      | Nat -> Nat
      | Z -> Z
      | S None -> S
      | S (Some t) -> App (S, (`Explicit, aux pren t))
      | Ind_nat s -> Term.rev_apps_explicit Ind_nat (List.map (aux pren) s)
    and aux_spine pren (t:term) : spine -> term = function
      | (i,u)::s -> App (aux_spine pren t s, (i, aux pren u))
      | [] -> t
    in
    aux pren t
  in
  let t = rename m pren t in
  let solution = eval [] @@ Term.abss (List.mapi (fun n i -> "x" ^ string_of_int (n+1), i) @@ List.rev @@ List.map fst s) t in
  Printf.printf "metavariable ?%d gets %s\n%!" m.id (to_string solution);
  m.value <- Some solution

let unify l t u =
  try unify l t u; true
  with Unification -> false
