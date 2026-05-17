open! Core
open Languages
module Types = Types
module Languages = Languages
module Desmos_vm_emulator = Desmos_vm_emulator

module For_testing = struct
  module Languages = Languages
  module Types = Types
end

module Passes = struct
  let check_function_defs = Pass_check_function_defs.compile

  let extract_fucntion_calls_and_defs =
    Pass_extract_function_calls_and_defs.compile

  let check_variables_scopes = Pass_check_variable_scopes.compile
  let rename_local_variables = Pass_rename_local_variables.compile
  let explicate_control = Pass_explicate_control.compile
  let analyze_call_liveness = Pass_analyze_call_liveness.compile
  let convert_functions_to_stack = Pass_convert_functions_to_stack.compile
  let make_program_counter_explicit = Pass_make_program_counter_explicit.compile
  let prepare_registers = Pass_prepare_registers.compile
  let generate_desmos_output = Pass_generate_desmos_output.compile
  let sanitize_register_names = Pass_sanitize_register_names.compile
end
