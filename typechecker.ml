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

(* TODO(astory): determine how best to do this *)
let munion' info map1 map2 =
  let add_item k v map =
    if StringMap.mem k map && StringMap.find k map <> v then
      raise (TypeException (info, "two different bindings?"))
    else
      StringMap.add k v map
  in
  StringMap.fold add_item map1 map2

let munion info =
  List.fold_left (munion' info) StringMap.empty

(* for now, just return type of underlying expression.  Later, need to modify ast*)
let rec typecheck_exp gamma expr =
  match expr with
    | EVar (info, id) ->
      let s = Id.string_of_t id in
      if StringMap.mem s gamma then
        (gamma, ConstraintSet.empty, StringMap.find s gamma)
      else
        raise (TypeException (info, "Unbound value " ^ s))
    | EApp (info, expr1, expr2) ->
      let resultant_type = fresh () in
      let (gamma1, constraints1, typ1) =
          typecheck_exp gamma expr1 in
      let (gamma2, constraints2, typ2) =
          typecheck_exp gamma expr2 in
      let constraints' =
          cunion [
              constraints1;
              constraints2;
              ConstraintSet.singleton
                  (typ1, TFunction (typ2, resultant_type))]
      in
      let gamma' = munion info [gamma1; gamma2] in
      (gamma', constraints', resultant_type)
    | EFun (info, param, expr) -> (gamma, ConstraintSet.empty, TUnit)
    | ELet (info, bind, expr) -> (gamma, ConstraintSet.empty, TUnit)
    | EAsc (info, expr, typ) -> (gamma, ConstraintSet.empty, TUnit)
    | EOver (info, op, exprs) -> (gamma, ConstraintSet.empty, TUnit)
    | EPair (info, expr1, expr2) -> (gamma, ConstraintSet.empty, TUnit)
    | ECase (info, expr1, pat_exprs) -> (gamma, ConstraintSet.empty, TUnit)
    | EUnit (info) -> (gamma, ConstraintSet.empty, TUnit)
    | EInteger (info, value) -> (gamma, ConstraintSet.empty, TUnit)
    | EChar (info, value) -> (gamma, ConstraintSet.empty, TUnit)
    | EString (info, value) -> (gamma, ConstraintSet.empty, TUnit)
    | EBool (info, value) -> (gamma, ConstraintSet.empty, TUnit)

let typecheck_decl (gamma, constraints) decl =
  match decl with
    | DLet (info, bind) ->
      (match bind with 
        | Bind (info, pattern, typopt, exp) ->
          let (gamma, constraints, t) =
              typecheck_exp gamma exp in
          (gamma, constraints))

    | DType (info, ids, id, labels) ->
      (*TODO(astory)*)
      (gamma, constraints)

let typecheck_modl = function
  | Modl (info, m, ds) ->
    let gamma = StringMap.empty in
    let constraints = ConstraintSet.empty in
    List.fold_left typecheck_decl (gamma, constraints) ds
