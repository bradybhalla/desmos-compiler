open! Core
open! Desmos_compiler
open! Languages
open! Types

let run_and_print_output prog =
  let result_register = Register.of_string "result" in
  let compiled_prog = Example_programs.compile_to_vm prog in
  let vm_state = Desmos_vm_emulator.run_until_done compiled_prog in
  let v = Hashtbl.find_exn vm_state.registers result_register in
  Printf.printf "%.1f\n" v

let%expect_test "fib" =
  Example_programs.prog_fib 0 |> run_and_print_output;
  [%expect {| 0.0 |}];
  Example_programs.prog_fib 2 |> run_and_print_output;
  [%expect {| 1.0 |}];
  Example_programs.prog_fib 12 |> run_and_print_output;
  [%expect {| 144.0 |}]

let%expect_test "gcd" =
  Example_programs.prog_gcd 6 9 |> run_and_print_output;
  [%expect {| 3.0 |}];
  Example_programs.prog_gcd 432 123 |> run_and_print_output;
  [%expect {| 3.0 |}];
  Example_programs.prog_gcd 432 1231 |> run_and_print_output;
  [%expect {| 1.0 |}]
