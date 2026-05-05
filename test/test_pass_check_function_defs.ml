open! Core
open! Desmos_compiler
open! Languages
open! Types

let check str =
  match str |> Utils.read_from_str |> Pass_check_function_defs.compile with
  | Ok _ -> print_endline "ok"
  | Error e -> print_endline (Error.to_string_hum e)

let%expect_test "return at the end" =
  check
    {|
    (def f (x) (
      (if (x > 1) (
          (return 1))
       else (
          (set x 2)))
      (return 2))
    )
    |};
  [%expect {| ok |}]

let%expect_test "all branches return" =
  check
    {|
    (def f (x) (
      (if (x > 1) (
          (return 1))
       else (
          (return 1))))
    )
    |};
  [%expect {| ok |}]

let%expect_test "early return in while" =
  check
    {|
    (def f (x) (
      (while (x == 1) (
        (return x)))
      (return 2)))
    |};
  [%expect {| ok |}]

let%expect_test "error on no return" =
  check {|
    (def f (x) (
      (set x 3)))
    |};
  [%expect {| ("function missing return" f) |}]

let%expect_test "error on branch missing return" =
  check
    {|
    (def f (x) (
      (if (x > 1) (
          (return 1))
       elif (x == 2) (
          (set y 1))
       else (
          (return 1)))))
    |};
  [%expect {| ("function missing return" f) |}]

let%expect_test "error on missing else branch with return" =
  check
    {|
    (def f (x) (
      (if (x > 1) (
          (return 1))
       elif (x == 2) (
          (return 1)))))
    |};
  [%expect {| ("function missing return" f) |}]

let%expect_test "error on only return in while" =
  check
    {|
    (def f (x) (
      (while (x == 1) (
        (return x)))))
    |};
  [%expect {| ("function missing return" f) |}]

let%expect_test "error on duplicate function names" =
  check
    {|
    (def f (x) (
      (return x)))

    (def f (y) (
      (return y)))
    |};
  [%expect {| ("duplicate function" f) |}]

let%expect_test "error on duplicate parameter names" =
  check {|
    (def f (x x) (
      (return x)))
    |};
  [%expect {| ("duplicate parameter" x f) |}]

let%expect_test "error on nested function definition" =
  check
    {|
    (def f (x) (
      (def g (y) (
        (return y)))
      (return x)))
    |};
  [%expect {| ("all functions must be at the toplevel" g) |}]

let%expect_test "error on non-toplevel function definition" =
  check
    {|
    (def f (x) (
      (return 0)))
    (def g (x) (
      (return 0)))

    (if (5 < 2) (
      (def g (x) (
        (return 0)))
    ))

    |};
  [%expect {| ("all functions must be at the toplevel" g) |}]

let%expect_test "error on return outside of function" =
  check {|
    (if (5 < 2) (
      (set x 2)
      (return x)
    ))

    |};
  [%expect {| "returns only allowed inside a function" |}]

let%expect_test "error on unreachable code after return in if" =
  check
    {|
    (def f (x) (
      (if (5 < 2) (
        (set x 2)
        (return x))
       else (
         (set x 2)
         (return x)
         (set x 3)))
    ))
    |};
  [%expect {| "unreachable code after return" |}]

let%expect_test "error on unreachable code after return in while" =
  check
    {|
    (def f (x) (
      (while (5 < 2) (
        (set x 2)
        (return x)
        (set x 2)
      ))
      (return x)
    ))
    |};
  [%expect {| "unreachable code after return" |}]
