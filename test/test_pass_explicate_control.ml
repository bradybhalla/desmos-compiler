open! Core
open Desmos_compiler
open Languages

let compile prog =
  prog |> Utils.read_from_str |> Cumulative_passes.explicate_control |> ok_exn
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
      (((label explicate_control_main_0) (body ())
        (control_flow
         (Jump (conds (((Bool true) explicate_control_if_statement_branch_2)))
          (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_branch_2) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_exit_1) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)) (desmos_vars ()) (desmos_plots ()))
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
      (((label explicate_control_main_0) (body ())
        (control_flow
         (Jump
          (conds
           (((Bool true) explicate_control_if_statement_branch_2)
            ((Bool false) explicate_control_if_statement_branch_3)))
          (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_branch_2) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_branch_3) (body ((Set x (Num 2))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_exit_1) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)) (desmos_vars ()) (desmos_plots ()))
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
      (((label explicate_control_main_0) (body ())
        (control_flow
         (Jump (conds (((Bool true) explicate_control_if_statement_branch_2)))
          (default explicate_control_if_statement_else_3))))
       ((label explicate_control_if_statement_branch_2) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_else_3) (body ((Set x (Num 2))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_exit_1) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)) (desmos_vars ()) (desmos_plots ()))
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
      (((label explicate_control_main_0) (body ())
        (control_flow
         (Jump
          (conds
           (((Bool true) explicate_control_if_statement_branch_2)
            ((Bool false) explicate_control_if_statement_branch_3)))
          (default explicate_control_if_statement_else_4))))
       ((label explicate_control_if_statement_branch_2) (body ((Set x (Num 1))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_branch_3) (body ((Set x (Num 2))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_else_4) (body ((Set x (Num 3))))
        (control_flow
         (Jump (conds ()) (default explicate_control_if_statement_exit_1))))
       ((label explicate_control_if_statement_exit_1) (body ())
        (control_flow Exit))))
     (global_registers (x)) (desmos_vars ()) (desmos_plots ()))
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
        ((entry_label explicate_control_f_entry_1) (params (1local_x_0))
         (blocks
          (((label explicate_control_f_entry_1) (body ())
            (control_flow
             (Jump
              (conds (((Bool true) explicate_control_if_statement_branch_3)))
              (default explicate_control_if_statement_else_4))))
           ((label explicate_control_if_statement_branch_3) (body ())
            (control_flow (Return (Num 1))))
           ((label explicate_control_if_statement_else_4) (body ())
            (control_flow
             (Jump
              (conds (((Bool false) explicate_control_if_statement_branch_6)))
              (default explicate_control_if_statement_else_7))))
           ((label explicate_control_if_statement_branch_6) (body ())
            (control_flow (Return (Num 2))))
           ((label explicate_control_if_statement_else_7) (body ())
            (control_flow (Return (Num 0))))
           ((label explicate_control_if_statement_exit_5) (body ())
            (control_flow
             (Jump (conds ()) (default explicate_control_if_statement_exit_2))))
           ((label explicate_control_if_statement_exit_2) (body ())
            (control_flow Exit))))
         (local_registers (1local_x_0))))))
     (main (((label explicate_control_main_0) (body ()) (control_flow Exit))))
     (global_registers ()) (desmos_vars ()) (desmos_plots ()))
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
      (((label explicate_control_main_0) (body ())
        (control_flow
         (Jump
          (conds
           (((Compare Lt (Register x) (Num 10)) explicate_control_while_entry_2)))
          (default explicate_control_while_end_1))))
       ((label explicate_control_while_entry_2)
        (body ((Set x (Add (Register x) (Num 1)))))
        (control_flow
         (Jump
          (conds
           (((Compare Lt (Register x) (Num 10)) explicate_control_while_entry_2)))
          (default explicate_control_while_end_1))))
       ((label explicate_control_while_end_1) (body ((Set x (Num 1))))
        (control_flow Exit))))
     (global_registers (x)) (desmos_vars ()) (desmos_plots ()))
    |}]
