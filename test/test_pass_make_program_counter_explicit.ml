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
      ((00link (Num 1.2345)) (00pc (Num 0)) (00ret (Num 1.2345)) (a (Num 1.2345))
       (b (Num 1.2345)))))
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
     (registers
      ((00link (Num 1.2345)) (00pc (Num 0)) (00ret (Num 1.2345))
       (c (Num 1.2345)))))
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
           ((00pc (Set (Add (Register 00pc) (Num 1)))) (00link Push)))
          (Instruction
           ((00link (Set (Add (Register 00pc) (Num 1))))
            (00pc (Set (LabelLineNumber explicate_control_function_entry_1)))))
          (Instruction ((00pc (Set (Add (Register 00pc) (Num 1)))) (00link Pop)))
          Exit)))
       ((label explicate_control_function_entry_1)
        (body
         ((Instruction ((00ret (Set (Num 0))) (00pc (Set (Register 00link))))))))))
     (registers
      ((00link (Num 1.2345)) (00pc (Num 0)) (00ret (Num 1.2345))
       (a (Num 1.2345)))))
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
       ((label explicate_control_function_entry_1)
        (body
         ((Instruction
           ((00ret (Set (Register 1local_b_0))) (00pc (Set (Register 00link))))))))))
     (registers
      ((00link (Num 1.2345)) (00pc (Num 0)) (00ret (Num 1.2345))
       (1local_b_0 (Num 1.2345)))))
    |}]
