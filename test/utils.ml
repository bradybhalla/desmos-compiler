open! Core
open Desmos_compiler
open Languages
open Types

let read_from_file file = file |> Sexp.load_sexps |> C_style_frontend.parse_ast
let read_from_str str = str |> Sexp.of_string_many |> C_style_frontend.parse_ast

let compile_frontend_to_vm prog =
  prog |> Passes.check_function_defs |> ok_exn
  |> Passes.extract_function_calls_and_defs |> Passes.check_variables_scopes
  |> ok_exn |> Passes.rename_local_variables |> Passes.explicate_control
  |> Passes.convert_functions_to_stack |> Passes.make_program_counter_explicit

let compile_vm_to_javascript prog =
  prog |> Passes.generate_desmos_output |> Passes.sanitize_register_names
  |> Desmos_output.to_pastable_javascript

let run_vm_and_get_ouptput ~output_reg_name prog =
  let emulator_state = Desmos_vm_emulator.run_until_done prog in
  Desmos_vm_emulator.inspect_register emulator_state
    (Register.of_string output_reg_name)
