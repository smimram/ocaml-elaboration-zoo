open Extlib

module P = Preterm
module T = Term
module V = Value

open Term

type preterm = P.t
type term = T.t
type value = V.t
type ty = V.t

exception Type_error of Pos.t * string

let type_error pos = Printf.ksprintf (fun s -> raise (Type_error (pos, s)))

(** Typing and evaluation contexts. *)
module Context = struct

  (** The contexts for type inference. *)
  type t =
    {
      environment : V.environment; (** the evaluation environment *)
      level : int; (** level for creating fresh variables for abstractions *)
      types : (string * V.ty) list; (** the typing environment *)
      bds : [`Bound | `Defined] list; (** whether variables of the environment are defined or bound (this is used for metavariables which only depend on bound variables) *)
    }

  (** Empty context. *)
  let empty =
    {
      environment = [];
      level = 0;
      types = [];
      bds = [];
    }

  (** Declare a bound variable of given type. *)
  let bind ctx x a =
    {
      environment = (V.var ctx.level)::ctx.environment;
      level = ctx.level + 1;
      types = (x,a)::ctx.types;
      bds = `Bound::ctx.bds;
    }

  (** Define a term to a given value and type. *)
  let define ctx x t a =
    {
      environment = t::ctx.environment;
      level = ctx.level + 1;
      types = (x,a)::ctx.types;
      bds = `Defined::ctx.bds;
    }

  (* close : (Γ : Con) → Val (Γ, x : A) B → Closure Γ A B *)
  let close ctx (t:value) : V.closure = ctx.environment, V.quote (ctx.level + 1) t
end

let fresh_meta (ctx:Context.t) =
  let m = Value.fresh_meta () in
  InsertedMeta (m.id, ctx.bds)

let rec infer (ctx:Context.t) (t:preterm) : term * ty =
  let pos = t.pos in
  match t.desc with
  | Let (x,a,t,u) ->
    let a = V.eval ctx.environment @@ check ctx a V.Type in
    let t = check ctx t a in
    let u, b = infer (Context.define ctx x (V.eval ctx.environment t) a) u in
    (* TODO: check this quote *)
    let a = V.quote ctx.level a in
    Let (x,a,t,u), b
  | Abs ((x,i,a),t) ->
    let a =
      match a with
      | Some a -> check ctx a V.Type
      | None -> fresh_meta ctx
    in
    let a = V.eval ctx.environment a in
    let ctx' = Context.bind ctx x a in
    let t, b = infer ctx' t in
    T.Abs ((x,i),t), V.Pi((x,i,a), Context.close ctx b)
  | App (t,(i,u)) ->
    let tpos = t.pos in
    let t, c =
      match i with
      | `Implicit -> infer ctx t
      | `Explicit ->
        (* Apply all implicit arguments to metavariables. *)
        let rec aux ((t:term),(c:ty)) =
          match V.force c with
          | Pi((_,`Implicit,_),(env,b)) ->
            let m = fresh_meta ctx in
            let m' = V.eval ctx.environment m in
            let t = App (t,(`Implicit,m)) in
            let c = V.eval (m'::env) b in
            aux (t, c)
          | _ -> t,c
        in
        aux @@ infer ctx t
    in
    let a,(env,b) =
      match c with
      | Pi ((_,i',a),(env,b)) ->
        if i <> i' then failwith "TODO: support implicit parameters";
        a,(env,b)
      | _ -> type_error tpos "term has type %s but a function was expected" @@ V.to_string c
    in
    let u = check ctx u a in
    App (t,(i,u)), V.eval ((V.eval ctx.environment u)::env) b
  | Var x ->
    let n, a =
      let rec aux n = function
        | (y,a)::l -> if x = y then (n,a) else aux (n+1) l
        | [] -> type_error pos "unbound variable: %s" x
      in
      aux 0 ctx.types
    in
    Var n, a
  | Pi ((x,i,a),b) ->
    let a =
      match a with
      | Some a -> check ctx a Type
      | None -> fresh_meta ctx
    in
    let b = check (Context.bind ctx x (V.eval ctx.environment a)) b Type in
    Pi ((x,i,a),b), Type
  | Type ->
    Type, Type
  | Hole ->
    let t = fresh_meta ctx in
    let a = V.eval ctx.environment @@ fresh_meta ctx in
    t, a

and check (ctx:Context.t) (t:preterm) (a:ty) : term =
  let pos = t.pos in
  let t, a' = infer ctx t in
  if not @@ V.unify ctx.Context.level a' a then type_error pos "expression has type %s but %s expected" (V.to_string a') (V.to_string a);
  t
