open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "normal instructions compile correctly" =
  let prog : Register_stack_instrs.t =
    let open Register_stack_instrs in
    let a = Register.of_string "a" in
    [
      {
        label = Label.of_string "normal_instrs";
        body =
          [
            GeneralizedSet [ (a, Set (Num 5.)) ];
            GeneralizedSet
              [ (Register.of_string "b", Set (Add (Register a, Num 1.))) ];
          ];
        control_flow = Exit;
      };
    ]
  in
  prog |> Pass_explicit_program_counter.compile
  |> Desmos_virtual_machine.sexp_of_t sexp_of_unit
  |> print_s;
  [%expect
    {|
    ((main
      (((label normal_instrs)
        (body
         ((Instruction
           ((00pc (Set (Add (Register 00pc) (Num 1)))) (a (Set (Num 5)))))
          (Instruction
           ((00pc (Set (Add (Register 00pc) (Num 1))))
            (b (Set (Add (Register a) (Num 1))))))
          Exit)))))
     (info ()))
    |}]

let%expect_test "jump compiles correctly" =
  let prog : Register_stack_instrs.t =
    let open Register_stack_instrs in
    [
      {
        label = Label.of_string "conditional_jump";
        body = [];
        control_flow =
          Jump
            {
              conds =
                [
                  ( Compare
                      (Compare_op.Eq, Register (Register.of_string "c"), Num 0.),
                    Label.of_string "target" );
                ];
              default = Label.of_string "default";
            };
      };
    ]
  in
  prog |> Pass_explicit_program_counter.compile
  |> Desmos_virtual_machine.sexp_of_t sexp_of_unit
  |> print_s;
  [%expect
    {|
    ((main
      (((label conditional_jump)
        (body
         ((Instruction
           ((00pc
             (Set
              (If_expr
               (conds
                (((Compare Eq (Register c) (Num 0)) (LabelLineNumber target))))
               (default (LabelLineNumber default))))))))))))
     (info ()))
    |}]

let%expect_test "JumpLink compiles correctly" =
  let prog : Register_stack_instrs.t =
    let open Register_stack_instrs in
    [
      {
        label = Label.of_string "jumplink_test";
        body =
          [
            GeneralizedSet [ (Register.of_string "a", Set (Num 1.)) ];
            JumpLink (Label.of_string "func");
          ];
        control_flow = Exit;
      };
    ]
  in
  prog |> Pass_explicit_program_counter.compile
  |> Desmos_virtual_machine.sexp_of_t sexp_of_unit
  |> print_s;
  [%expect
    {|
    ((main
      (((label jumplink_test)
        (body
         ((Instruction
           ((00pc (Set (Add (Register 00pc) (Num 1)))) (a (Set (Num 1)))))
          (Instruction
           ((00link (Set (Add (Register 00pc) (Num 1))))
            (00pc (Set (LabelLineNumber func)))))
          Exit)))))
     (info ()))
    |}]

let%expect_test "return compiles correctly" =
  let prog : Register_stack_instrs.t =
    let open Register_stack_instrs in
    [
      {
        label = Label.of_string "return_test";
        body = [];
        control_flow = Return (Register (Register.of_string "b"));
      };
    ]
  in
  prog |> Pass_explicit_program_counter.compile
  |> Desmos_virtual_machine.sexp_of_t sexp_of_unit
  |> print_s;
  [%expect
    {|
    ((main
      (((label return_test)
        (body
         ((Instruction
           ((00ret (Set (Register b))) (00pc (Set (Register 00link))))))))))
     (info ()))
    |}]
