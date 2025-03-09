(** Pre-terms are what we get out of the parser. *)

open Common
open Extlib

(** A variable. *)
type var = string

(** An expression. *)
type t =
  {
    pos : Pos.t;
    desc : desc;
  }

(** The contents of an expression. *)
and desc =
  | Let of var * ty * t * t
  | Abs of (var * icit * ty option) * t (** λ-abstraction *)
  | App of t * (icit * t)
  | Var of var
  | Pi of (var * icit * ty option) * t
  | Type (** the type of types *)
  | Hole

and ty = t

let mk ?pos desc =
  let pos = Option.value ~default:Pos.dummy pos in
  { pos; desc }

(** Non-dependent arrow. *)
let arr ?pos a b =
  mk ?pos (Pi (("_", `Explicit, Some a), b))

(** Multiple abstractions. *)
let abss ?pos a e =
  let pos = Option.value ~default:e.pos pos in
  let rec aux = function
    | [] -> e
    | x::l -> mk ~pos (Abs (x, aux l))
  in
  aux a

(** Multiple pi types. *)
let pis ?pos args a =
  let pos = Option.value ~default:a.pos pos in
  let rec aux = function
    | [] -> a
    | x::l -> mk ~pos (Pi (x, aux l))
  in
  aux args
 
let rec to_string ?(pa=false) e =
  let pa s = if pa then "("^s^")" else s in
  match e.desc with
  | Let (x,a,t,u) ->
    Printf.sprintf "let %s : %s = %s in\n%s" x (to_string a) (to_string t) (to_string u)
  | Abs ((x,i,a),t) ->
    let arg =
      let a = match a with Some a -> " : " ^ to_string a | None -> "" in
      icit_pa i (x ^ a) in
    pa (Printf.sprintf "fun %s -> %s" arg (to_string t))
  | App (f,(i,e)) ->
    let e =
      match i with
      | `Explicit -> to_string ~pa:true e
      | `Implicit -> "{" ^ to_string e ^ "}"
    in
    pa (Printf.sprintf "%s %s" (to_string f) e)
  | Pi ((x,i,a),b) ->
    let a = match a with Some a -> " : " ^ to_string a | None -> "" in
    let arg = icit_pa i (x ^ a) in
    pa (Printf.sprintf "%s -> %s" arg (to_string b))
  | Var x -> x
  | Hole -> "_"
  | Type -> "type"
