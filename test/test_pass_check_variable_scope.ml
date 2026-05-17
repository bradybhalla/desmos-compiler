open! Core
open! Desmos_compiler
open! Languages
open! Types

let check str =
  match
    str |> Utils.read_from_str |> Passes.check_function_defs |> ok_exn
    |> Passes.extract_function_calls_and_defs |> Passes.check_variables_scopes
  with
  | Ok _ -> print_endline "ok"
  | Error e -> print_endline (Error.to_string_hum e)

let%expect_test "normal variable usage" =
  check {|
    (decl x)
    (set x 1)
    |};
  [%expect {| ok |}]

let%expect_test "errors on set before decared" =
  check {|
    (set x 1)
    (decl x)
    |};
  [%expect {| ("variable not declared in scope" x) |}]

let%expect_test "errors on using undeclared" =
  check {|
    (decl y)
    (set y x)
    (decl y)
    |};
  [%expect {| ("variable not declared in scope" x) |}]

let%expect_test "errors on double declared" =
  check {|
    (decl x)
    (decl x)
    |};
  [%expect {| ("variable already declared in this scope" x) |}]

let%expect_test "declared globally and set inside if statement is fine" =
  check
    {|
    (decl x)
    (set x 0)
    (if true ((set x 1)) else ((set x 2)))
    |};
  [%expect {| ok |}]

let%expect_test "declared globally and set inside while loop is fine" =
  check {|
    (decl x)
    (set x 0)
    (while true ((set x 1)))
    |};
  [%expect {| ok |}]

let%expect_test
    "declared globally and redeclared in every branch of if statement is fine" =
  check
    {|
    (decl x)
    (if true ((decl x) (set x 1)) else ((decl x) (set x 2)))
    |};
  [%expect {| ok |}]

let%expect_test "errors if only declared in if branch but set in else" =
  check {|
    (if true ((decl x) (set x 1)) else ((set x 2)))
    |};
  [%expect {| ("variable not declared in scope" x) |}]

let%expect_test "function using its parameter without declaring is fine" =
  check {|
    (def foo (x) ((return x)))
    |};
  [%expect {| ok |}]

let%expect_test "function using global variables is fine" =
  check {|
    (decl x)
    (set x 0)
    (def foo () ((return x)))
    |};
  [%expect {| ok |}]

let%expect_test "errors on function using variables declared in if" =
  check
    {|
    (if true ((decl x) (set x 1)) else ())
    (def foo () ((return x)))
    |};
  [%expect {| ("variable not declared in scope" x) |}]

let%expect_test "errors on globally using variable declared in function" =
  check {|
    (def foo () ((decl x) (return x)))
    (set x 1)
    |};
  [%expect {| ("variable not declared in scope" x) |}]
