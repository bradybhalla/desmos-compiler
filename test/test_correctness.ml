open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "fib" =
  Utils.read_from_file "programs/fib_12.sexp"
  |> Utils.compile_frontend_to_vm
  |> Utils.run_vm_and_get_ouptput ~output_reg_name:"result"
  |> Printf.printf "%f";
  [%expect {| 144.000000 |}]

let%expect_test "basic features" =
  Utils.read_from_file "programs/basic_language_features.sexp"
  |> Utils.compile_frontend_to_vm
  |> Utils.run_vm_and_get_ouptput ~output_reg_name:"a"
  |> Printf.printf "%f";
  [%expect {| 6.000000 |}]
