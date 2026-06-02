open! Core
open Desmos_compiler

let check str =
  match str |> Utils.read_from_str |> Passes.check_function_defs_and_plotting with
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
  [%expect {| ("function missing return" (func_name f)) |}]

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
  [%expect {| ("function missing return" (func_name f)) |}]

let%expect_test "error on missing else branch with return" =
  check
    {|
    (def f (x) (
      (if (x > 1) (
          (return 1))
       elif (x == 2) (
          (return 1)))))
    |};
  [%expect {| ("function missing return" (func_name f)) |}]

let%expect_test "error on only return in while" =
  check
    {|
    (def f (x) (
      (while (x == 1) (
        (return x)))))
    |};
  [%expect {| ("function missing return" (func_name f)) |}]

let%expect_test "error on duplicate function names" =
  check
    {|
    (def f (x) (
      (return x)))

    (def f (y) (
      (return y)))
    |};
  [%expect {| ("duplicate function" (dup f)) |}]

let%expect_test "error on duplicate parameter names" =
  check {|
    (def f (x x) (
      (return x)))
    |};
  [%expect {| ("duplicate parameter" (dup x) (func_name f)) |}]

let%expect_test "error on nested function definition" =
  check
    {|
    (def f (x) (
      (def g (y) (
        (return y)))
      (return x)))
    |};
  [%expect {| ("all functions must be at the toplevel" (name g)) |}]

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
  [%expect {| ("all functions must be at the toplevel" (name g)) |}]

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

let%expect_test "desmos_decl at toplevel is ok" =
  check {|
    (#decl x 0)
    |};
  [%expect {| ok |}]

let%expect_test "desmos_point at toplevel is ok" =
  check {|
    (@point x y)
    |};
  [%expect {| ok |}]

let%expect_test "desmos_line at toplevel is ok" =
  check {|
    (@line (from 0 0) (to 1 1))
    |};
  [%expect {| ok |}]

let%expect_test "error on desmos_decl inside function" =
  check
    {|
    (def f (x) (
      (#decl y 0)
      (return x)))
    |};
  [%expect {| "desmos statements must be at the toplevel" |}]

let%expect_test "error on desmos_point inside function" =
  check
    {|
    (def f (x) (
      (@point x 0)
      (return x)))
    |};
  [%expect {| "desmos statements must be at the toplevel" |}]

let%expect_test "error on desmos_line inside function" =
  check
    {|
    (def f (x) (
      (@line (from 0 0) (to 1 1))
      (return x)))
    |};
  [%expect {| "desmos statements must be at the toplevel" |}]

let%expect_test "error on desmos_decl inside if" =
  check {|
    (if (5 < 2) (
      (#decl x 0)
    ))
    |};
  [%expect {| "desmos statements must be at the toplevel" |}]

let%expect_test "error on desmos_point inside while" =
  check {|
    (while (5 < 2) (
      (@point 1 2)
    ))
    |};
  [%expect {| "desmos statements must be at the toplevel" |}]

let%expect_test "error on function call in desmos_point" =
  check
    {|
    (def f (x) (
      (return x)))
    (@point (f 1) 2)
    |};
  [%expect {| "function calls not allowed in desmos plotting expressions" |}]

let%expect_test "error on function call in desmos_line" =
  check
    {|
    (def f (x) (
      (return x)))
    (@line (from (f 1) 0) (to 1 1))
    |};
  [%expect {| "function calls not allowed in desmos plotting expressions" |}]

let%expect_test "no function call in desmos_point is ok" =
  check
    {|
    (def f (x) (
      (return x)))
    (@point (1 + 2) (3 * 4))
    |};
  [%expect {| ok |}]
