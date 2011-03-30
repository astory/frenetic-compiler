(******************************************************************************)
(* The Frenetic Project                                                       *)
(* frenetic@frenetic-lang.org                                                 *)
(******************************************************************************)
(* Licensed to the Frenetic Project by one or more contributors. See the      *)
(* NOTICE file distributed with this work for additional information          *)
(* regarding copyright and ownership. The Frenetic Project licenses this      *)
(* file to you under the following license.                                   *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided the following conditions are met:     *)
(* - Redistributions of source code must retain the above copyright           *)
(*   notice, this list of conditions and the following disclaimer.            *)
(* - Redistributions in binary form must reproduce the above copyright        *)
(*   notice, this list of conditions and the following disclaimer in          *)
(*   the documentation or other materials provided with the distribution.     *)
(* - The names of the copyright holds and contributors may not be used to     *)
(*   endorse or promote products derived from this work without specific      *)
(*   prior written permission.                                                *)
(*                                                                            *)
(* Unless required by applicable law or agreed to in writing, software        *)
(* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  *)
(* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   *)
(* LICENSE file distributed with this work for specific language governing    *)
(* permissions and limitations under the License.                             *)
(******************************************************************************)
(* /src/compiler/typechecker.ml                                               *)
(* Type checking for the Frenetic syntax                                      *)
(* $Id$ *)
(******************************************************************************)
(*TODO(astory): useful errors*)
module StringMap = Map.Make (String)
include Syntax
module ConstraintSet = Set.Make (
  struct
    let compare = compare
    type t = typ * typ
  end )

exception TypeException of (Info.t * string)

type gammat = typ StringMap.t 
type constrt = ConstraintSet.t

(* TODO(astory): make this actually produce fresh variables *)
let fresh () =
  TVar (Info.M ("what do I put here?"), None, "a fresh variable")

let cunion = List.fold_left (ConstraintSet.union) ConstraintSet.empty

let cadd t1 t2 = ConstraintSet.add (t1, t2)

let ceq t1 t2 = ConstraintSet.singleton (t1, t2)

let c0 = ConstraintSet.empty

let fv var typ =
    (*TODO(astory)*)
    false

let rec unify cs =
    if ConstraintSet.is_empty cs then StringMap.empty
    else
        let (s,t) = ConstraintSet.choose cs in
        let cs' = ConstraintSet.remove (s,t) cs in
        if s == t then
            unify cs'
        else match (s,t) with
          | (TVar(_,_,var), _) when not (fv var t) ->
            StringMap.add var t (unify (cadd s t cs')) (*TODO(astory): ask about*)
          | (_, TVar(_,_,var)) when not (fv var s) ->
            StringMap.add var s (unify (cadd t s cs')) (*TODO(astory): ask about*)
          | (TFunction(s1,s2),TFunction(t1,t2)) ->
            unify (cunion [cs'; ceq s1 t1; ceq s2 t2])
          | _ -> raise (TypeException (Info.M (""), "Could not unify"))

let rec assign_types (gamma, constraints) info pattern t =
    match pattern with
      | PWild (info') -> (gamma, constraints)
      | PUnit (info') -> (gamma, cadd TUnit t constraints)
      | PBool (info', _) -> (gamma, cadd TBool t constraints)
      | PInteger (info', _) -> (gamma, cadd TInteger t constraints)
      | PString (info', _) -> (gamma, cadd TString t constraints)
      | PVar (_, (info', mo, s), typ_opt) ->
        (match typ_opt with
          | Some t' -> (StringMap.add s t gamma, cadd t' t constraints)
          | None -> (StringMap.add s t gamma, constraints))
      (*| PData (info, id, pattern) *)
      | PPair (info, p1, p2) ->
          (match t with
            | TProduct(t1, t2) ->
                let (gamma', constraints') =
                    assign_types (gamma, constraints) info p1 t1 in
                assign_types (gamma', constraints') info p2 t2
            | _ -> raise (TypeException(info, "type is not a product")))
      | _ -> raise (TypeException(info, "PData not supported"))


(* for now, just return type of underlying expression.  Later, need to modify ast*)
let rec typecheck_exp gamma expr =
  match expr with
    | EVar (info, id) ->
      let s = Id.string_of_t id in
      if StringMap.mem s gamma then
        (StringMap.find s gamma, c0)
      else
        raise (TypeException (info, "Unbound value " ^ s))
    | EApp (info, expr1, expr2) ->
      let resultant_type = fresh () in
      let (typ1, constraints1) =
          typecheck_exp gamma expr1 in
      let (typ2, constraints2) =
          typecheck_exp gamma expr2 in
      let constraints' =
          cunion [
              constraints1;
              constraints2;
              ceq typ1 (TFunction (typ2, resultant_type))]
      in
      (resultant_type, constraints')
    | EFun (info, param, expr) ->
        (match param with
          | Param (param_info, pattern, typ) ->
            let t1 = fresh() in
            let constraints = (match typ with
              | Some t -> ceq t1 t
              | None -> c0) in
            let (gamma', constraints') =
                (* TODO(astory): make work for pairs without explicitness *)
                assign_types (gamma, constraints) param_info pattern t1
            in
            let (t, constraints'') = typecheck_exp gamma' expr in
            (TFunction(t1, t), cunion [constraints''; constraints']))
    | ELet (info, bind, expr) ->
      (match bind with
        | Bind (info, pattern, typ, expr') ->
          let (expr'_t, constraints) = typecheck_exp gamma expr' in
          let (gamma', constraints') =
                assign_types (gamma, constraints) info pattern expr'_t in
          typecheck_exp gamma' expr)
    | EAsc (info, expr, typ) ->
      let (expr_t, constraints) = typecheck_exp gamma expr in
      (expr_t, cadd expr_t typ constraints)
    | EOver (info, op, exprs) ->
        raise (TypeException(info, "Overloaded operators not implemented"))

    | EPair (info, expr1, expr2) ->
      let (typ1, constraints1) =
          typecheck_exp gamma expr1 in
      let (typ2, constraints2) =
          typecheck_exp gamma expr2 in
      (TProduct (typ1, typ2), cunion [constraints1; constraints2])
    | ECase (info, expr1, pat_exprs) ->
      raise (TypeException(info, "Case operator not implemented"))

    | EUnit    (info)        -> (TUnit,    ConstraintSet.empty)
    | EBool    (info, value) -> (TBool,    ConstraintSet.empty)
    | EInteger (info, value) -> (TInteger, ConstraintSet.empty)
    | EChar    (info, value) -> (TChar,    ConstraintSet.empty)
    | EString  (info, value) -> (TString,  ConstraintSet.empty)

let typecheck_decl (gamma, constraints) decl =
  match decl with
    | DLet (info, bind) ->
      (match bind with 
        | Bind (info, pattern, typopt, exp) ->
          let (t, constraints) = typecheck_exp gamma exp in
          assign_types (gamma, constraints) info pattern t)
    | DType (info, ids, id, labels) ->
      (*TODO(astory)*)
      (gamma, constraints)

let typecheck_modl = function
  | Modl (info, m, ds) ->
    let gamma = StringMap.empty in
    let constraints = ConstraintSet.empty in
    let (_, constraints') =
        List.fold_left typecheck_decl (gamma, constraints) ds in
    let sigma = unify constraints' in
    ()
