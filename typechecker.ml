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

let cunion =
  List.fold_left (ConstraintSet.union) ConstraintSet.empty

(* for now, just return type of underlying expression.  Later, need to modify ast*)
let rec typecheck_exp gamma expr =
  match expr with
    | EVar (info, id) ->
      let s = Id.string_of_t id in
      if StringMap.mem s gamma then
        (ConstraintSet.empty, StringMap.find s gamma)
      else
        raise (TypeException (info, "Unbound value " ^ s))
    | EApp (info, expr1, expr2) ->
      let resultant_type = fresh () in
      let (constraints1, typ1) =
          typecheck_exp gamma expr1 in
      let (constraints2, typ2) =
          typecheck_exp gamma expr2 in
      let constraints' =
          cunion [
              constraints1;
              constraints2;
              ConstraintSet.singleton
                  (typ1, TFunction (typ2, resultant_type))]
      in
      (constraints', resultant_type)
    | EFun (info, param, expr) -> (ConstraintSet.empty, TUnit)
    | ELet (info, bind, expr) ->
      (match bind with
        | Bind (info, pattern, typ, expr') ->
          let (constraints, expr'_t) = typecheck_exp gamma expr' in
          match pattern with
            | PVar (_, (info, mo, s), _) ->
              let gamma' = StringMap.add s expr'_t gamma in
              typecheck_exp gamma' expr
            | _ -> raise (TypeException(info, "pattern not a variable")))
    | EAsc (info, expr, typ) ->
      let (constraints, expr_t) = typecheck_exp gamma expr in
      (cunion [constraints; ConstraintSet.singleton((expr_t, typ))],
        expr_t)
    | EOver (info, op, exprs) ->
        raise (TypeException(info, "Overloaded operators not implemented"))

    | EPair (info, expr1, expr2) ->
      let (constraints1, typ1) =
          typecheck_exp gamma expr1 in
      let (constraints2, typ2) =
          typecheck_exp gamma expr2 in
      (cunion [constraints1; constraints2], TProduct (typ1, typ2))
    | ECase (info, expr1, pat_exprs) ->
      raise (TypeException(info, "Case operator not implemented"))

    | EUnit    (info)        -> (ConstraintSet.empty, TUnit)
    | EBool    (info, value) -> (ConstraintSet.empty, TBool)
    | EInteger (info, value) -> (ConstraintSet.empty, TInteger)
    | EChar    (info, value) -> (ConstraintSet.empty, TChar)
    | EString  (info, value) -> (ConstraintSet.empty, TString)

let typecheck_decl (gamma, constraints) decl =
  match decl with
    | DLet (info, bind) ->
      (match bind with 
        | Bind (info, pattern, typopt, exp) ->
          let (constraints, t) =
              typecheck_exp gamma exp in
          match pattern with
            | PVar (_, (info', mo, s), _) ->
              (StringMap.add s t gamma, constraints)
            | _ -> raise (TypeException(info, "pattern is not a variable")))

    | DType (info, ids, id, labels) ->
      (*TODO(astory)*)
      (gamma, constraints)

let typecheck_modl = function
  | Modl (info, m, ds) ->
    let gamma = StringMap.empty in
    let constraints = ConstraintSet.empty in
    List.fold_left typecheck_decl (gamma, constraints) ds
