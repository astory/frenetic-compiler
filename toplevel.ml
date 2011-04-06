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
(* /src/compiler/toplevel.ml                                                  *)
(* Real front-end                                                             *)
(* $Id$ *)
(******************************************************************************)

let sprintf = Printf.sprintf

let arg_spec = 
  [ ("-debug", Arg.String Prefs.add_debug_flag, ": print debugging information") ]

let usage prog = sprintf "Usage:\n    %s [options] F.fnet [F.fnet...]\n" prog

let anon_cell = ref []
let anon_arg x = anon_cell := (x::!anon_cell)

let go' prog () = 
  Arg.parse arg_spec anon_arg (usage prog);
  match !anon_cell with 
    | [fn] -> 
      begin 
        let _ = Lexer.setup fn in 
        let lexbuf = Lexing.from_string (Util.read fn) in       
        let ast = 
          try Parser.modl Lexer.main lexbuf with 
            | Parsing.Parse_error ->
              (Error.error
                 (fun () -> Util.format "@[%s:@ syntax@ error@\n@]"
                   (Info.string_of_t (Lexer.info lexbuf)))) in 
		print_string (Py.format_modl ast);
        ()
      end
    | _ -> 
      begin 
        Util.format "@[%s@]" (usage prog); 
        exit 2  
      end
    
let go prog =
  try 
    Unix.handle_unix_error 
      (fun () -> Error.exit_if_error (go' prog))
      ();
    exit 0
  with e -> 
    Util.format "@[Uncaught exception %s@]" (Printexc.to_string e); 
    exit 2
