open! Core
open Desmos_compiler
open Languages

let compile str =
  str |> Utils.read_from_str |> Cumulative_passes.make_program_counter_explicit
  |> ok_exn |> Desmos_virtual_machine.sexp_of_t |> print_s

let%expect_test "normal instructions compile correctly" =
  compile {|
  (decl a)
  (decl b)
  (set a 5)
  (set b (a + 1)) |};
  [%expect
    {|
    ((main
      (((label explicate_control_main_0)
        (body
         ((Instruction
           ((00pc (Set (Add (Register 00pc) (Num 1)))) (a (Set (Num 5)))))
          (Instruction
           ((00pc (Set (Add (Register 00pc) (Num 1))))
            (b (Set (Add (Register a) (Num 1))))))
          Exit)))))
     (registers
      ((00pc (Num 0)) (00ret (Num 1.2345)) (a (Num 1.2345)) (b (Num 1.2345))))
     (desmos_vars ()) (desmos_plots ()))
    |}]

let%expect_test "jump compiles correctly" =
  compile {|
  (decl c)
  (if (c == 0) ()) |};
  [%expect
    {|
    ((main
      (((label explicate_control_main_0)
        (body
         ((Instruction
           ((00pc
             (Set
              (If_expr
               (conds
                (((Compare Eq (Register c) (Num 0))
                  (LabelLineNumber explicate_control_if_statement_branch_2))))
               (default (LabelLineNumber explicate_control_if_statement_exit_1))))))))))
       ((label explicate_control_if_statement_branch_2)
        (body
         ((Instruction
           ((00pc
             (Set
              (If_expr (conds ())
               (default (LabelLineNumber explicate_control_if_statement_exit_1))))))))))
       ((label explicate_control_if_statement_exit_1) (body (Exit)))))
     (registers ((00pc (Num 0)) (00ret (Num 1.2345)) (c (Num 1.2345))))
     (desmos_vars ()) (desmos_plots ()))
    |}]

let%expect_test "JumpLink compiles correctly" =
  compile {|
  (def func () ((return 0)))
  (decl a)
  (set a 1)
  (func) |};
  [%expect
    {|
    ((main
      (((label explicate_control_main_0)
        (body
         ((Instruction
           ((00pc (Set (Add (Register 00pc) (Num 1)))) (a (Set (Num 1)))))
          (Instruction
           ((00pc
             (PushExprAndSet (push (LabelLineNumber convert_funcs_to_stack_0))
              (set (LabelLineNumber explicate_control_func_entry_1)))))))))
       ((label convert_funcs_to_stack_0)
        (body ((Instruction ((00pc (Set (Add (Register 00pc) (Num 1)))))) Exit)))
       ((label explicate_control_func_entry_1)
        (body ((Instruction ((00ret (Set (Num 0))) (00pc Pop))))))))
     (registers ((00pc (Num 0)) (00ret (Num 1.2345)) (a (Num 1.2345))))
     (desmos_vars ()) (desmos_plots ()))
    |}]

let%expect_test "return compiles correctly" =
  compile {|
  (def return_test () (
    (decl b)
    (return b)
  )) |};
  [%expect
    {|
    ((main
      (((label explicate_control_main_0) (body (Exit)))
       ((label explicate_control_return_test_entry_1)
        (body ((Instruction ((00ret (Set (Register 1local_b_0))) (00pc Pop))))))))
     (registers ((00pc (Num 0)) (00ret (Num 1.2345)) (1local_b_0 (Num 1.2345))))
     (desmos_vars ()) (desmos_plots ()))
    |}]
