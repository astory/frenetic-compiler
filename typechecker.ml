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
include Syntax
open Id
open Map

module StringMap = Map.Make (
  struct
    type t = string
    let compare = compare
  end )
module ConstraintSet = Set.Make (
  struct
    let compare = compare
    type t = typ
  end )

exception TypeException of (Info.t * string)

(* for now, just return type of underlying expression.  Later, need to modify ast*)
let rec typecheck_exp (gamma, constraints) exp =
  match exp with
    | EVar (info, id) ->
      let s = Id.string_of_t id in
      if StringMap.mem s gamma then
        (gamma, constraints, StringMap.find s gamma)
      else
        raise TypeException(info, "Unbound value " ^ s)
    | EApp (info, exp1, exp2) -> (gamma, constraints)
    | EFun (info, param, exp) -> (gamma, constraints)
    | ELet (info, bind, exp) -> (gamma, constraints)
    | EAsc (info, exp, typ) -> (gamma, constraints)
    | EOver (info, op, exps) -> (gamma, constraints)
    | EPair (info, exp1, exp2) -> (gamma, constraints)
    | ECase (info, exp1, pat_exps) -> (gamma, constraints)
    | EUnit (info) -> (gamma, constraints)
    | EInteger (info, value) -> (gamma, constraints)
    | EChar (info, value) -> (gamma, constraints)
    | EString (info, value) -> (gamma, constraints)
    | EBool (info, value) -> (gamma, constraints)

let typecheck_decl (gamma, constraints) decl =
  match decl with
    | DLet (info, bind) ->
      match bind with 
        | Bind (info, pattern, typopt, exp) ->
          let (gamma, constraints, t) =
              typecheck_exp (gamma, constraints) exp in
          (gamma, constraints)

    | DType (info, ids, id, labels) ->
      (*TODO(astory)*)
      (gamma, constraints)

let typecheck_modl = function
  | Modl (info, m, ds) ->
    let gamma = StringMap.empty in
    let constraints = ConstraintSet.empty in
    List.fold_left typecheck_decl (gamma, constraints) ds
