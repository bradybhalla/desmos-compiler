open! Core
open! Desmos_compiler
open! Languages
open! Types

let check str =
  match
    str |> Utils.read_from_str |> Pass_check_function_defs.compile |> ok_exn
    |> Pass_extract_function_calls_and_defs.compile
    |> Pass_check_variable_scopes.compile
  with
  | Ok _ -> print_endline "ok"
  | Error e -> print_endline (Error.to_string_hum e)
