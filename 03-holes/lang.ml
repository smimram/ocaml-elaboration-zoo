module RawTerm = struct
  (** A raw term (this is what we get out of the parser). *)
  type t =
    | Var of string
    | Abs of string * t
    | App of t * t
    | U
    | Pi of string * ty * ty
    | Hole
    | Let of string * ty * t * t

  (** A type (only for clarity, it's a term). *)
  and ty = t

  (** Abstract over multiple variables. *)
  let rec abs l t =
    match l with
    | x::l -> Abs (x, abs l t)
    | [] -> t

  (** String representation. *)
  let rec to_string = function
    | Var x -> x
    | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (to_string t)
    | Let (x, a, t, u) -> Printf.sprintf "let %s : %s = %s;\n%s" x (to_string a) (to_string t) (to_string u)
    | U -> "𝕌"
    | Pi (x, a, t) -> Printf.sprintf "((%s : %s) → %s)" x (to_string a) (to_string t)
    | Hole -> "_"
end

module Term = struct
  (** A term (a raw term massaged a bit to have de Bruijn indices and metavariables). *)
  type t =
    | Var of int
    | Abs of string * t
    | App of t * t
    | U
    | Pi of string * ty * ty
    | Let of string * ty * t * t
    | Meta of int

  (** A type (only for clarity, it's a term). *)
  and ty = t

  let rec to_string = function
    | Var i -> Printf.sprintf "x%d" i
    | Abs (x, t) -> Printf.sprintf "λ%s.%s" x (to_string t)
    | App (t, u) -> Printf.sprintf "(%s %s)" (to_string t) (to_string u)
    | U -> "𝕌"
    | Pi (x, a, b) -> Printf.sprintf "(%s : %s) -> %s" x (to_string a) (to_string b)
    | Let (x, a, t, u) -> Printf.sprintf "let %s : %s = %s;\n%s" x (to_string a) (to_string t) (to_string u)
    | Meta i -> Printf.sprintf "?%d" i

  let rec apps t = function
    | u::l -> apps (App (t, u)) l
    | [] -> t

  (** Generate a fresh meta-variable. *)
  let metavariable =
    (* Note that the integer is not very relevant here, it's only used for printing purposes. *)
    let n = ref (-1) in
    fun () ->
      incr n;
      Meta !n
end

(** A value. *)
type t =
  | VApp of int * t list (** A neutral value consisting of a variable applied to a list of terms (the first application is the one at the end of the list, ie xᵢt₁…tₙ is represented by (i,[tₙ;…;t₁])). *)
  | MApp of metavariable * t list (** A neutral value consisting of a metavariable applied to terms (as above, the top of the list is the outermost application).  *)
  | Abs of environment * string * Term.t (** A λ-abstraction in a closure. *)
  | Pi of environment * string * t * Term.t (** A Π-type in a closure. *)
  | U (** The universe. *)

(** A metavariable consisting of its name (an integer) and its value when it is known. *)
and metavariable = int * t option ref

(** An environment. *)
and environment = t list

let rec to_string ?(environment=false) = function
  | VApp (i, l) -> List.fold_right (fun t s -> Printf.sprintf "%s %s" s (to_string t)) l (Printf.sprintf "x%d" i)
  | MApp (m, l) ->
    (* Print the value of the metavariable when it is known *)
    let tm = match !(snd m) with None -> "" | Some t -> "=" ^ to_string t in
    List.fold_right (fun t s -> Printf.sprintf "%s %s" s (to_string t)) l (Printf.sprintf "?%d%s" (fst m) tm)
  | Abs (_, x, t) -> Printf.sprintf "λ%s.%s" x (Term.to_string t)
  | Pi (env, x, a, b) ->
    let env = if environment then string_of_environment env else "" in
    Printf.sprintf "(%s : %s) -> %s%s" x (to_string a) (Term.to_string b) env
  | U -> "𝕌"

and string_of_environment env =
  List.map to_string env |> String.concat ", " |> fun s -> "["^s^"]"

(** A variable term. *)
let var i = VApp (i, [])

(** All the metavariables. It should only be accessed with the functions below (excepting for debugging purposes). *)
let metavariables = ref []

(** Get the term corresponding to a metavariable. Those are stored in a global environment since we want to share the same reference for all of them. *)
let metavariable =
  fun i : metavariable ->
    match List.assoc_opt i !metavariables with
    | Some v -> v
    | None ->
      let v = i, ref None in
      metavariables := (i, v) :: !metavariables;
      v

(** Compute weak head normal form. *)
let rec eval (env : environment) (t : Term.t) : t =
  (* Printf.printf "eval: %s in [%s]\n%!" (Term.to_string t) (List.map to_string env |> String.concat ", "); *)
  match t with
  | Var i -> (try List.nth env i with Failure _ | Invalid_argument _ -> failwith ("Evaluation: inexistent variable " ^ string_of_int i))
  | Abs (x, t) -> Abs (env, x, t)
  | App (t, u) ->
    let u = eval env u in
    (
      match eval env t with
      | Abs (env, _, t) -> eval (u::env) t
      | VApp (x, l) -> VApp (x, u::l)
      | MApp (x, l) -> MApp (x, u::l)
      | _ -> assert false
    )
  | U -> U
  | Pi (x, a, b) ->
    Pi (env, x, eval env a, b)
  | Let (_, _, t, u) ->
    let t = eval env t in
    eval (t::env) u
  | Meta m -> MApp (metavariable m, [])
  
(** Reify normal form. *)
let rec quote l : t -> Term.t = function
  | VApp (i, uu) ->
    (* The outermost argument is the first one in the list. *)
    List.fold_right (fun u t -> Term.App (t, quote l u)) uu (Var (l-1 - i))
  | MApp ((i,_), tt) ->
    let tt = List.rev_map (quote l) tt in
    Term.apps (Meta i) tt
  | Abs (env, x, t) ->
    let t = eval ((var l)::env) t in
    Abs (x, quote (l+1) t)
  | Pi (env, x, a, b) ->
    let a = quote l a in
    let b = eval ((var l)::env) b |> quote (l+1) in
    Pi (x, a, b)
  | U -> U

(** Compute the normal form of a term. *)
let normalize env t =
  eval env t |> quote (List.length env)

(** Apply a term to another. *)
let app t u =
  (* Printf.printf "app %s to %s\n%!" (to_string t) (to_string u); *)
  match t with
  | VApp (i, l) -> VApp (i, u::l)
  | MApp (m, l) -> MApp (m, u::l)
  | Abs (env, _, t) -> eval (u::env) t
  | _ -> assert false

(** Perform a series of applications. *)
let rec apps t = function
  | u::l -> apps (app t u) l
  | [] -> t

(** Resolve the value of metavariables when it's known. Types should always be forced before matching on them. *)
let force = function
  | MApp ((_, m), l) when !m <> None -> apps (Option.get !m) (List.rev l)
  | t -> t

(** Create a metavariable applied to the λ-abstracted variables in the environment. *)
let fresh_metavariable env tenv menv l =
  (* List.iter2 (fun b (x,_) -> print_string ((if b then "+" else "-") ^ x ^ " ")) menv tenv; *)
  (* print_newline (); *)
  let vars = List.mapi (fun i b -> if b then Some (var (l-1 - i)) else None) menv in
  let vars = List.filter_map (fun x -> x) vars in
  (* This could be optimized: we could only use λ-abstracted variables
     (for instance by adding a boolean in tenv). *)
  (* let vars = List.init l (fun i -> var (l-1 - i)) in *)
  let a = eval env (Term.metavariable ()) in
  let m = apps a vars in
  (* Printf.printf "meta: %s\n%!" (to_string m); *)
  (* Printf.printf "env: %s\n%!" (List.map to_string env |> String.concat ", "); *)
  m

exception Unification

(** Unify two terms, i.e. assign values to metavariables so that they become equal. *)
let rec unify l t u =
  (* Printf.printf "unify %s with %s\n%!" (to_string ~environment:true t) (to_string ~environment:true u); *)
  match force t, force u with
  | Abs (env, _, t), Abs (env', _, u) ->
    let t = eval ((var l)::env) t in
    let u = eval ((var l)::env') u in
    unify (l+1) t u
  | Abs (env, _, t), ((VApp _ | MApp _) as u) ->
    (* η-conversion *)
    let t = eval ((var l)::env) t in
    let u = app u (var l) in
    unify (l+1) t u
  | (VApp _ | MApp _), Abs _ -> unify l u t
  | Pi (env, _, a, b), Pi (env', _, a', b') ->
    unify l a a';
    (* Printf.printf "b: %s vs %s\n%!" (Term.to_string b) (Term.to_string b'); *)
    (* Printf.printf "env: [%s] vs [%s]\n%!" (List.map to_string env |> String.concat ", ") (List.map to_string env' |> String.concat ", "); *)
    let b = eval ((var l)::env) b in
    let b' = eval ((var l)::env') b' in
    unify (l+1) b b'
  | VApp (i, tt), VApp (i', tt') ->
    if i <> i' || List.length tt <> List.length tt' then raise Unification;
    List.iter2 (unify l) tt tt'
  | MApp (m, tt), MApp (m', tt') when fst m = fst m' ->
    if List.length tt <> List.length tt' then raise Unification;
    List.iter2 (unify l) tt tt'
  | MApp ((m,r), tt), u ->
    (* The metavariable is applied to n arguments x₀ … xₙ₋₁, so that tt is
       [xₙ₋₁; …; x₀]. We ensure that those are pairwise distinct variables and
       map each of them to its level in the context of the solution (i.e. xᵢ is
       mapped to i). *)
    let n = List.length tt in
    let s =
      List.fold_left
        (fun (k, s) t ->
           match force t with
           | VApp (j, []) when not (List.mem_assoc j s) -> k-1, (j, k)::s
           | _ -> raise Unification
        ) (n-1, []) tt
      |> snd
    in
    (* Translate a value at level l into a term in the context of the solution,
       which has d variables, s mapping levels of the former to levels of the
       latter. We also perform the occurs check along the way. *)
    let rec subst l d s t =
      match force t with
      | VApp (i, uu) ->
        let i =
          match List.assoc_opt i s with
          | Some i -> d-1 - i
          | None -> raise Unification (* variable escaping scope *)
        in
        List.fold_right (fun u t -> Term.App (t, subst l d s u)) uu (Term.Var i)
      | MApp ((m', _), uu) ->
        if m' = m then raise Unification; (* occurs check *)
        List.fold_right (fun u t -> Term.App (t, subst l d s u)) uu (Term.Meta m')
      | Abs (env, x, t) ->
        let t = eval ((var l)::env) t |> subst (l+1) (d+1) ((l,d)::s) in
        Abs (x, t)
      | Pi (env, x, a, b) ->
        let a = subst l d s a in
        let b = eval ((var l)::env) b |> subst (l+1) (d+1) ((l,d)::s) in
        Pi (x, a, b)
      | U -> U
    in
    let abss n t =
      let rec aux l =
        if l = n then t
        else Term.Abs ("x" ^ string_of_int l, aux (l+1))
      in
      aux 0
    in
    let solution = abss n (subst l n s u) |> eval [] in
    (* Printf.printf "solution of %s = %s is %s\n%!" (to_string t) (to_string u) (to_string solution); *)
    r := Some solution
  | _, MApp _ -> unify l u t
  | U, U -> ()
  | _ -> raise Unification

exception Typing of string

(** Check that a raw term has a given type and transform it into a term along the way. The environment env binds variables to values, the typing environment tenv provides the type of variables and menv indicates whether we should keep a variable as argument for metavariables (currently we only keep λ-abstractions but not variables declared with let). *)
let rec check env tenv menv l (t : RawTerm.t) a : Term.t =
  match t, force a with
  | Abs (x, t), Pi (env', _, a, b) ->
    let b = eval ((var l)::env') b in
    let t = check ((var l)::env) ((x,a)::tenv) (true::menv) (l+1) t b in
    Abs (x, t)
  | Let (x, a, t, u), b ->
    let a = check env tenv menv l a U in
    let va = eval env a in
    let t = check env tenv menv l t va in
    let vt = eval env t in
    let u = check (vt::env) ((x,va)::tenv) (false::menv) (l+1) u b in
    Term.Let (x, a, t, u)
  | Hole, _ ->
    (* Metavariables are not typed. *)
    quote l (fresh_metavariable env tenv menv l)
  | t, a ->
    (* The term cannot be checked, try to infer instead. *)
    let t, b = infer env tenv menv l t in
    (* Printf.printf "could not check %s : %s, unify with %s instead\n%!" (Term.to_string t) (to_string a) (to_string b); *)
    unify l a b; 
    t

(** Infer the type of a term and convert the raw term to a term along the way. *)
and infer env tenv menv l (t : RawTerm.t) : Term.t * t =
  match t with
  | Var x ->
    let rec aux i = function
      | (x',a)::l -> if x' = x then Term.Var i, a else aux (i+1) l
      | [] -> raise (Typing ("unbound variable " ^ x))
    in
    aux 0 tenv
  | Abs (x, t) ->
    let a = fresh_metavariable env tenv menv l in
    let t, b = infer ((var l)::env) ((x,a)::tenv) (true::menv) (l+1) t in
    let b = quote (l+1) b in
    Abs (x, t), Pi(env, x, a, b)
  | App (t, u) ->
    let t, c = infer env tenv menv l t in
    (* Printf.printf "t is %s : %s\n%!" (Term.to_string t) (to_string c); *)
    (
      match force c with
      | Pi (env', _, a, b) ->
        let u = check env tenv menv l u a in
        let b =
          let u = eval env u in
          eval (u::env') b
        in
        App(t, u), b
      | c ->
        let a = fresh_metavariable env tenv menv l in
        let b = fresh_metavariable ((var l)::env) (("x",a)::tenv) (true::menv) (l+1) in
        let b = quote (l+1) b in
        unify l c (Pi (env, "x", a, b));
        let u = check env tenv menv l u a in
        App(t, u), eval ((eval env u)::env) b
    )
  | Pi (x, a, b) ->
    let a = check env tenv menv l a U in
    let va = eval env a in
    let b = check ((var l)::env) ((x,va)::tenv) (true::menv) (l+1) b U in
    Pi (x, a, b), U
  | U -> U, U (* type in type *)
  | Let (x, a, t, u) ->
    let a = check env tenv menv l a U in
    let va = eval env a in
    (* Printf.printf "let type %s => %s\n%!" (Term.to_string a) (to_string va); *)
    let t = check env tenv menv l t va in
    let vt = eval env t in
    let u, b = infer (vt::env) ((x,va)::tenv) (false::menv) (l+1) u in
    Let (x, a, t, u), b
  | Hole ->
    (* Metavariables are not typed (see the README), so nothing relates the
       metavariable for the type to the one for the term. *)
    let a = fresh_metavariable env tenv menv l in
    let t = quote l (fresh_metavariable env tenv menv l) in
    t, a
