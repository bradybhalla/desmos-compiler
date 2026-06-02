open! Core
module Types = Types
module Languages = Languages
module Desmos_vm_emulator = Desmos_vm_emulator

module Passes = struct
  let check_function_defs_and_plotting = Pass_check_function_defs_and_plotting.compile

  let extract_functions_and_plotting =
    Pass_extract_functions_and_plotting.compile

  let check_variables_scopes = Pass_check_variable_scopes.compile
  let rename_local_variables = Pass_rename_local_variables.compile
  let explicate_control = Pass_explicate_control.compile
  let convert_functions_to_stack = Pass_convert_functions_to_stack.compile
  let compress_instructions = Pass_compress_instructions.compile
  let make_program_counter_explicit = Pass_make_program_counter_explicit.compile
  let generate_desmos_output = Pass_generate_desmos_output.compile
  let sanitize_register_names = Pass_sanitize_register_names.compile
  let generate_javascript = Pass_generate_javascript.compile
end

module Cumulative_passes = struct
  open Or_error.Let_syntax

  let check_function_defs_and_plotting prog = prog |> Passes.check_function_defs_and_plotting

  let extract_functions_and_plotting prog =
    prog |> check_function_defs_and_plotting >>| Passes.extract_functions_and_plotting

  let check_variables_scopes prog =
    prog |> extract_functions_and_plotting >>= Passes.check_variables_scopes

  let rename_local_variables prog =
    prog |> check_variables_scopes >>| Passes.rename_local_variables

  let explicate_control prog =
    prog |> rename_local_variables >>| Passes.explicate_control

  let convert_functions_to_stack prog =
    prog |> explicate_control >>| Passes.convert_functions_to_stack

  let compress_instructions prog =
    prog |> convert_functions_to_stack >>| Passes.compress_instructions

  let make_program_counter_explicit prog =
    prog |> compress_instructions >>| Passes.make_program_counter_explicit

  let generate_desmos_output prog =
    prog |> make_program_counter_explicit >>| Passes.generate_desmos_output

  let sanitize_register_names prog =
    prog |> generate_desmos_output >>| Passes.sanitize_register_names

  let generate_javascript prog =
    prog |> sanitize_register_names >>| Passes.generate_javascript
end
