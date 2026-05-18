open! Core
open Desmos_compiler
open Languages
open Types

let compile_frontend_to_vm prog =
  prog |> Passes.check_function_defs |> ok_exn
  |> Passes.extract_function_calls_and_defs |> Passes.check_variables_scopes
  |> ok_exn |> Passes.rename_local_variables |> Passes.explicate_control
  |> Passes.convert_functions_to_stack |> Passes.make_program_counter_explicit

let desmos_cmd =
  Command.basic
    ~summary:
      "Compile a program to Desmos expressions. The output will be a \
       JavaScript command that can be pasted into the console on Desmos."
    (let%map_open.Command file = anon ("file" %: string) in
     fun () ->
       let prog = file |> Sexp.load_sexps |> C_style_frontend.parse_ast in
       let js =
         prog |> compile_frontend_to_vm |> Passes.generate_desmos_output
         |> Passes.sanitize_register_names
         |> Languages.Desmos_output.to_pastable_javascript
       in
       print_endline js)

let emulator_cmd =
  Command.basic ~summary:"Compile a program and run it with the emulator."
    (let%map_open.Command file = anon ("file" %: string)
     and output_reg =
       flag "-reg"
         (optional_with_default "result" string)
         ~doc:"REG Register to inspect after execution"
     in
     fun () ->
       let prog = file |> Sexp.load_sexps |> C_style_frontend.parse_ast in
       let vm_prog = prog |> compile_frontend_to_vm in
       let emulator_state = Desmos_vm_emulator.run_until_done vm_prog in
       let result =
         Desmos_vm_emulator.inspect_register emulator_state
           (Register.of_string output_reg)
       in
       printf "%f\n" result)

let to_parse_ast = C_style_frontend.parse_ast

let to_check_function_defs prog =
  prog |> to_parse_ast |> Passes.check_function_defs

let to_extract_function_calls prog =
  prog |> to_check_function_defs |> ok_exn
  |> Passes.extract_function_calls_and_defs

let to_check_variable_scopes prog =
  prog |> to_extract_function_calls |> Passes.check_variables_scopes

let to_rename_local_variables prog =
  prog |> to_check_variable_scopes |> ok_exn |> Passes.rename_local_variables

let to_explicate_control prog =
  prog |> to_rename_local_variables |> Passes.explicate_control

let to_convert_to_stack prog =
  prog |> to_explicate_control |> Passes.convert_functions_to_stack

let to_make_pc_explicit prog =
  prog |> to_convert_to_stack |> Passes.make_program_counter_explicit

let to_generate_desmos prog =
  prog |> to_make_pc_explicit |> Passes.generate_desmos_output

let to_sanitize prog =
  prog |> to_generate_desmos |> Passes.sanitize_register_names

let passes : (string * (Sexp.t list -> Sexp.t)) list =
  [
    ( "frontend",
      fun prog ->
        prog |> to_parse_ast |> [%sexp_of: [ `Unchecked ] C_style_frontend.t] );
    ( "check-function-defs",
      fun prog ->
        prog |> to_check_function_defs
        |> [%sexp_of: [ `Checked_function_defs ] C_style_frontend.t Or_error.t]
    );
    ( "extract-function-calls",
      fun prog ->
        prog |> to_extract_function_calls
        |> [%sexp_of: [ `Unchecked ] C_style_separated_functions.t] );
    ( "check-variable-scopes",
      fun prog ->
        prog |> to_check_variable_scopes
        |> [%sexp_of:
             [ `Checked_variable_scopes ] C_style_separated_functions.t
             Or_error.t] );
    ( "rename-local-variables",
      fun prog ->
        prog |> to_rename_local_variables |> [%sexp_of: C_style_registers.t] );
    ( "explicate-control",
      fun prog ->
        prog |> to_explicate_control |> [%sexp_of: Register_func_instrs.t] );
    ( "convert-to-stack",
      fun prog ->
        prog |> to_convert_to_stack |> [%sexp_of: Register_stack_instrs.t] );
    ( "make-pc-explicit",
      fun prog ->
        prog |> to_make_pc_explicit |> [%sexp_of: Desmos_virtual_machine.t] );
    ( "generate-desmos",
      fun prog ->
        prog |> to_generate_desmos
        |> [%sexp_of: [ `Unsanitized ] Desmos_output.t] );
    ( "sanitize",
      fun prog ->
        prog |> to_sanitize |> [%sexp_of: [ `Sanitized ] Desmos_output.t] );
  ]

let pass_arg = Command.Arg_type.of_alist_exn passes

let debug_cmd =
  Command.basic
    ~summary:"Run the compiler up to a pass and print the output as a sexp"
    (let%map_open.Command pass =
       flag "-pass" (required pass_arg) ~doc:"PASS Final pass to run"
     and file = anon ("file" %: string) in
     fun () ->
       file |> Sexp.load_sexps |> pass |> Sexp.to_string_hum |> print_endline)

let () =
  Command_unix.run
    (Command.group ~summary:"Desmos compiler"
       [
         ("desmos", desmos_cmd); ("emulator", emulator_cmd); ("debug", debug_cmd);
       ])
