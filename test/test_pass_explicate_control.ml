open! Core
open Desmos_compiler
open Languages

let compile prog =
  prog |> Utils.read_from_str |> Cumulative_passes.explicate_control
  |> [%sexp_of: Register_func_instrs.t] |> print_s

let%expect_test "if" =
  compile {|
  (decl x)
  (if true (
    (set x 1)
  ))
  (set x 1) |};
  [%expect
    {|
    ((functions ())
     (main
      (((label explicate_control_0_main) (body ())
        (control_flow
         (Jump (conds (((Bool true) explicate_control_2_if_statement_branch)))
          (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_1_if_statement_exit) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)))
    |}]

let%expect_test "if / elif" =
  compile
    {|
  (decl x)
  (if true (
    (set x 1)
  ) elif false (
    (set x 2)
  ))
  (set x 1) |};
  [%expect
    {|
    ((functions ())
     (main
      (((label explicate_control_0_main) (body ())
        (control_flow
         (Jump
          (conds
           (((Bool true) explicate_control_2_if_statement_branch)
            ((Bool false) explicate_control_3_if_statement_branch)))
          (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_3_if_statement_branch) (body ((Set x (Num 2))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_1_if_statement_exit) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)))
    |}]

let%expect_test "if / else" =
  compile
    {|
  (decl x)
  (if true (
    (set x 1)
  ) else (
    (set x 2)
  ))
  (set x 1) |};
  [%expect
    {|
    ((functions ())
     (main
      (((label explicate_control_0_main) (body ())
        (control_flow
         (Jump (conds (((Bool true) explicate_control_2_if_statement_branch)))
          (default explicate_control_3_if_statement_else))))
       ((label explicate_control_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_3_if_statement_else) (body ((Set x (Num 2))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_1_if_statement_exit) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)))
    |}]

let%expect_test "if / elif / else" =
  compile
    {|
  (decl x)
  (if true (
    (set x 1)
  ) elif false (
    (set x 2)
  ) else (
    (set x 3)
  )) |};
  [%expect
    {|
    ((functions ())
     (main
      (((label explicate_control_0_main) (body ())
        (control_flow
         (Jump
          (conds
           (((Bool true) explicate_control_2_if_statement_branch)
            ((Bool false) explicate_control_3_if_statement_branch)))
          (default explicate_control_4_if_statement_else))))
       ((label explicate_control_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_3_if_statement_branch) (body ((Set x (Num 2))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_4_if_statement_else) (body ((Set x (Num 3))))
        (control_flow
         (Jump (conds ()) (default explicate_control_1_if_statement_exit))))
       ((label explicate_control_1_if_statement_exit) (body ())
        (control_flow Exit))))
     (global_registers (x)))
    |}]

let%expect_test "function where all branches return (nested if)" =
  (* the current behavior is that this creates some empty unreachable blocks *)
  compile
    {|
  (def f (x) (
    (if true (
      (return 1)
    ) else (
      (if false (
        (return 2)
      ) else (
        (return 0)
      ))
    ))
  )) |};
  [%expect
    {|
    ((functions
      ((f
        ((entry_label explicate_control_1_function_entry)
         (params (1rename_local_vars_0))
         (blocks
          (((label explicate_control_1_function_entry) (body ())
            (control_flow
             (Jump
              (conds (((Bool true) explicate_control_3_if_statement_branch)))
              (default explicate_control_4_if_statement_else))))
           ((label explicate_control_3_if_statement_branch) (body ())
            (control_flow (Return (Num 1))))
           ((label explicate_control_4_if_statement_else) (body ())
            (control_flow
             (Jump
              (conds (((Bool false) explicate_control_6_if_statement_branch)))
              (default explicate_control_7_if_statement_else))))
           ((label explicate_control_6_if_statement_branch) (body ())
            (control_flow (Return (Num 2))))
           ((label explicate_control_7_if_statement_else) (body ())
            (control_flow (Return (Num 0))))
           ((label explicate_control_5_if_statement_exit) (body ())
            (control_flow
             (Jump (conds ()) (default explicate_control_2_if_statement_exit))))
           ((label explicate_control_2_if_statement_exit) (body ())
            (control_flow Exit))))
         (local_registers (1rename_local_vars_0))))))
     (main (((label explicate_control_0_main) (body ()) (control_flow Exit))))
     (global_registers ()))
    |}]

let%expect_test "while loop" =
  compile
    {|
  (decl x)
  (while (x < 10) (
    (set x (x + 1))
  ))
  (set x 1) |};
  [%expect
    {|
    ((functions ())
     (main
      (((label explicate_control_0_main) (body ())
        (control_flow
         (Jump
          (conds
           (((Compare Lt (Register x) (Num 10)) explicate_control_2_while_entry)))
          (default explicate_control_1_while_end))))
       ((label explicate_control_2_while_entry)
        (body ((Set x (Add (Register x) (Num 1)))))
        (control_flow
         (Jump
          (conds
           (((Compare Lt (Register x) (Num 10)) explicate_control_2_while_entry)))
          (default explicate_control_1_while_end))))
       ((label explicate_control_1_while_end) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)))
    |}]
