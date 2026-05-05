open! Core
open! Desmos_compiler
open! Languages
open! Types

let read_from_file file = file |> Sexp.load_sexps |> C_style_frontend.parse_ast
let read_from_str str = str |> Sexp.of_string_many |> C_style_frontend.parse_ast

let compile_frontend_to_vm prog =
  prog |> Pass_check_function_defs.compile |> ok_exn
  |> Pass_extract_function_calls_and_defs.compile
  |> Pass_check_variable_scopes.compile |> Pass_rename_local_variables.compile
  |> Pass_explicate_control.compile |> Pass_analyze_call_liveness.compile
  |> Pass_convert_functions_to_stack.compile
  |> Pass_make_program_counter_explicit.compile
  |> Pass_prepare_registers.compile

let compile_vm_to_javascript prog =
  prog |> Pass_generate_desmos_output.compile
  |> Desmos_output.to_pastable_javascript

let run_vm_and_get_ouptput ~output_reg_name prog =
  let emulator_state = Desmos_vm_emulator.run_until_done prog in
  Hashtbl.find_exn emulator_state.registers (Register.of_string output_reg_name)
