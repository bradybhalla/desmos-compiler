open! Core
open Desmos_compiler
open Languages
open Types
open Or_error.Let_syntax

let desmos_cmd =
  Command.basic
    ~summary:
      "Compile a program to Desmos expressions. The output will be a \
       JavaScript command that can be pasted into the console on Desmos."
    (let%map_open.Command file = anon ("file" %: string) in
     fun () ->
       let prog = file |> Sexp.load_sexps |> C_style_frontend.parse_ast in
       let js =
         prog |> Cumulative_passes.sanitize_register_names
         >>| Languages.Desmos_output.to_pastable_javascript
       in
       match js with
       | Ok js -> print_endline js
       | Error e -> print_s [%sexp (e : Error.t)])

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
       let result =
         let%map vm_prog =
           Cumulative_passes.make_program_counter_explicit prog
         in
         let emulator_state = Desmos_vm_emulator.run_until_done vm_prog in
         Desmos_vm_emulator.inspect_register emulator_state
           (Register.of_string output_reg)
       in
       match result with
       | Ok result -> printf "%f\n" result
       | Error e -> print_s [%sexp (e : Error.t)])

let parse = C_style_frontend.parse_ast

let passes : (string * (Sexp.t list -> Sexp.t)) list =
  [
    ( "parse",
      fun prog -> prog |> parse |> [%sexp_of: [ `Unchecked ] C_style_frontend.t]
    );
    ( "check-func-defs",
      fun prog ->
        prog |> parse |> Cumulative_passes.check_function_defs
        |> [%sexp_of: [ `Checked_function_defs ] C_style_frontend.t Or_error.t]
    );
    ( "extract-funcs",
      fun prog ->
        prog |> parse |> Cumulative_passes.extract_function_calls_and_defs
        |> [%sexp_of: [ `Unchecked ] C_style_separated_functions.t Or_error.t]
    );
    ( "check-scopes",
      fun prog ->
        prog |> parse |> Cumulative_passes.check_variables_scopes
        |> [%sexp_of:
             [ `Checked_variable_scopes ] C_style_separated_functions.t
             Or_error.t] );
    ( "rename-locals",
      fun prog ->
        prog |> parse |> Cumulative_passes.rename_local_variables
        |> [%sexp_of: C_style_registers.t Or_error.t] );
    ( "explicate-control",
      fun prog ->
        prog |> parse |> Cumulative_passes.explicate_control
        |> [%sexp_of: Register_func_instrs.t Or_error.t] );
    ( "convert-to-stack",
      fun prog ->
        prog |> parse |> Cumulative_passes.convert_functions_to_stack
        |> [%sexp_of: Register_stack_instrs.t Or_error.t] );
    ( "make-pc-explicit",
      fun prog ->
        prog |> parse |> Cumulative_passes.make_program_counter_explicit
        |> [%sexp_of: Desmos_virtual_machine.t Or_error.t] );
    ( "generate-desmos",
      fun prog ->
        prog |> parse |> Cumulative_passes.generate_desmos_output
        |> [%sexp_of: [ `Unsanitized ] Desmos_output.t Or_error.t] );
    ( "sanitize-registers",
      fun prog ->
        prog |> parse |> Cumulative_passes.sanitize_register_names
        |> [%sexp_of: [ `Sanitized ] Desmos_output.t Or_error.t] );
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
