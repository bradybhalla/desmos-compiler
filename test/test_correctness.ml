open! Core
open Or_error.Let_syntax

let compile_and_run ~file ~reg =
  Utils.read_from_file file |> Utils.compile_frontend_to_vm
  >>| Utils.run_vm_and_get_ouptput ~output_reg_name:reg
  |> [%sexp_of: float Or_error.t] |> print_s

let%expect_test "basic features" =
  compile_and_run ~file:"programs/basic_language_features.sexp" ~reg:"a";
  [%expect {| 6.000000 |}]

let%expect_test "fib 12 with a while loop" =
  compile_and_run ~file:"programs/fib_12.sexp" ~reg:"result";
  [%expect {| 144.000000 |}]

let%expect_test "fib 8 with a slow recursive function" =
  compile_and_run ~file:"programs/fib_8_slow.sexp" ~reg:"n";
  [%expect {| 21.000000 |}]

let%expect_test "highest number in collatz(27)" =
  compile_and_run ~file:"programs/collatz.sexp" ~reg:"highest";
  [%expect {| 9232.000000 |}]

let%expect_test "short circuiting" =
  compile_and_run ~file:"programs/short_circuiting.sexp" ~reg:"count";
  [%expect {| 17.000000 |}]

let%expect_test "variable scopes" =
  compile_and_run ~file:"programs/variable_scopes.sexp" ~reg:"y";
  [%expect {| 8.000000 |}]
