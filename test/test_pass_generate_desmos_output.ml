open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "0 instructions" =
  let prog : Desmos_virtual_machine.expr Register.Map.t Desmos_virtual_machine.t
      =
    let open Desmos_virtual_machine in
    {
      main = [ { label = Label.of_string "main"; body = [] } ];
      info = Register.Map.of_alist_exn [ (program_counter_reg, Num 0.) ];
    }
  in
  let output = prog |> Pass_generate_desmos_output.compile in
  output |> Desmos_output.sexp_of_t |> print_s;
  [%expect
    {|
    ((program_action ((conds ()) (default ((00pc (Register 00pc))))))
     (init_registers ((00pc (Num 0)) (00pcStack (ListLiteral ((Num 1.2345)))))))
    |}];
  output |> Desmos_output.to_pastable_javascript |> print_endline;
  [%expect
    {| Calc.setExpressions([{latex: "R_{00pc}\\to R_{00pc}"}, {latex: "R_{00pc}=0"}, {latex: "R_{00pcStack}=\\left[1.2345\\right]"}]) |}]

let%expect_test "multiple instructions" =
  let prog : Desmos_virtual_machine.expr Register.Map.t Desmos_virtual_machine.t
      =
    let open Desmos_virtual_machine in
    let a = Register.of_string "a" in
    let b = Register.of_string "b" in
    {
      main =
        [
          {
            label = Label.of_string "main";
            body =
              [
                Instruction [ (a, Set (Num 1.)); (b, Set (Num 2.)) ];
                Instruction [ (a, Set (Register a)) ];
                Exit;
              ];
          };
        ];
      info =
        Register.Map.of_alist_exn
          [ (program_counter_reg, Num 0.); (a, Num 0.); (b, Num 0.) ];
    }
  in
  let output = prog |> Pass_generate_desmos_output.compile in
  output |> Desmos_output.sexp_of_t |> print_s;
  [%expect
    {|
    ((program_action
      ((conds
        (((Compare Eq (Register 00pc) (Num 0)) ((a (Num 1)) (b (Num 2))))
         ((Compare Eq (Register 00pc) (Num 1)) ((a (Register a))))
         ((Compare Eq (Register 00pc) (Num 2)) ((00pc (Num -1))))))
       (default ((00pc (Register 00pc))))))
     (init_registers
      ((00pc (Num 0)) (00pcStack (ListLiteral ((Num 1.2345)))) (a (Num 0))
       (aStack (ListLiteral ((Num 1.2345)))) (b (Num 0))
       (bStack (ListLiteral ((Num 1.2345)))))))
    |}];
  output |> Desmos_output.to_pastable_javascript |> print_endline;
  [%expect
    {| Calc.setExpressions([{latex: "\\left\\{R_{00pc} = 0: \\left(R_{a}\\to 1, R_{b}\\to 2\\right), R_{00pc} = 1: R_{a}\\to R_{a}, R_{00pc} = 2: R_{00pc}\\to -1, R_{00pc}\\to R_{00pc}\\right\\}"}, {latex: "R_{00pc}=0"}, {latex: "R_{00pcStack}=\\left[1.2345\\right]"}, {latex: "R_{a}=0"}, {latex: "R_{aStack}=\\left[1.2345\\right]"}, {latex: "R_{b}=0"}, {latex: "R_{bStack}=\\left[1.2345\\right]"}]) |}]

let%expect_test "push and pop" =
  let prog : Desmos_virtual_machine.expr Register.Map.t Desmos_virtual_machine.t
      =
    let open Desmos_virtual_machine in
    let a = Register.of_string "a" in
    let b = Register.of_string "b" in
    let c = Register.of_string "c" in
    let d = Register.of_string "d" in
    {
      main =
        [
          {
            label = Label.of_string "main";
            body =
              [
                Instruction
                  [
                    (a, Push);
                    (b, Pop);
                    (c, Set (Num 1.));
                    (d, PushAndSet (Num 1.));
                  ];
                Exit;
              ];
          };
        ];
      info =
        Register.Map.of_alist_exn
          [
            (program_counter_reg, Num 0.);
            (a, Num 0.);
            (b, Num 0.);
            (c, Num 0.);
            (d, Num 0.);
          ];
    }
  in
  let output = prog |> Pass_generate_desmos_output.compile in
  output |> Desmos_output.sexp_of_t |> print_s;
  [%expect
    {|
    ((program_action
      ((conds
        (((Compare Eq (Register 00pc) (Num 0))
          ((aStack (ListJoin (Register aStack) (Register a)))
           (b (ListIndex (Register bStack) (ListLength (Register bStack))))
           (bStack
            (ListSlice (Register bStack) (Num 1)
             (Sub (ListLength (Register bStack)) (Num 1))))
           (c (Num 1)) (dStack (ListJoin (Register dStack) (Register d)))
           (d (Num 1))))
         ((Compare Eq (Register 00pc) (Num 1)) ((00pc (Num -1))))))
       (default ((00pc (Register 00pc))))))
     (init_registers
      ((00pc (Num 0)) (00pcStack (ListLiteral ((Num 1.2345)))) (a (Num 0))
       (aStack (ListLiteral ((Num 1.2345)))) (b (Num 0))
       (bStack (ListLiteral ((Num 1.2345)))) (c (Num 0))
       (cStack (ListLiteral ((Num 1.2345)))) (d (Num 0))
       (dStack (ListLiteral ((Num 1.2345)))))))
    |}];
  output |> Desmos_output.to_pastable_javascript |> print_endline;
  [%expect
    {| Calc.setExpressions([{latex: "\\left\\{R_{00pc} = 0: \\left(R_{aStack}\\to \\operatorname{join}\\left(R_{aStack}, R_{a}\\right), R_{b}\\to R_{bStack}\\left[\\operatorname{length}\\left(R_{bStack}\\right)\\right], R_{bStack}\\to R_{bStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{bStack}\\right) - 1\\right)\\right], R_{c}\\to 1, R_{dStack}\\to \\operatorname{join}\\left(R_{dStack}, R_{d}\\right), R_{d}\\to 1\\right), R_{00pc} = 1: R_{00pc}\\to -1, R_{00pc}\\to R_{00pc}\\right\\}"}, {latex: "R_{00pc}=0"}, {latex: "R_{00pcStack}=\\left[1.2345\\right]"}, {latex: "R_{a}=0"}, {latex: "R_{aStack}=\\left[1.2345\\right]"}, {latex: "R_{b}=0"}, {latex: "R_{bStack}=\\left[1.2345\\right]"}, {latex: "R_{c}=0"}, {latex: "R_{cStack}=\\left[1.2345\\right]"}, {latex: "R_{d}=0"}, {latex: "R_{dStack}=\\left[1.2345\\right]"}]) |}]

let%expect_test "nested if_expr" =
  let prog : Desmos_virtual_machine.expr Register.Map.t Desmos_virtual_machine.t
      =
    let open Desmos_virtual_machine in
    let a = Register.of_string "a" in
    {
      main =
        [
          {
            label = Label.of_string "main";
            body =
              [
                Instruction
                  [
                    ( a,
                      Set
                        (If_expr
                           {
                             conds = [];
                             default =
                               If_expr
                                 {
                                   conds =
                                     [
                                       (Compare (Gt, Num 1., Num 1.), Num 1.);
                                       (Compare (Gt, Num 1., Num 1.), Num 2.);
                                     ];
                                   default = Num 3.;
                                 };
                           }) );
                  ];
                Exit;
              ];
          };
        ];
      info =
        Register.Map.of_alist_exn [ (program_counter_reg, Num 0.); (a, Num 0.) ];
    }
  in
  let output = prog |> Pass_generate_desmos_output.compile in
  output |> Desmos_output.sexp_of_t |> print_s;
  [%expect
    {|
    ((program_action
      ((conds
        (((Compare Eq (Register 00pc) (Num 0))
          ((a
            (If_expr (conds ())
             (default
              (If_expr
               (conds
                (((Compare Gt (Num 1) (Num 1)) (Num 1))
                 ((Compare Gt (Num 1) (Num 1)) (Num 2))))
               (default (Num 3))))))))
         ((Compare Eq (Register 00pc) (Num 1)) ((00pc (Num -1))))))
       (default ((00pc (Register 00pc))))))
     (init_registers
      ((00pc (Num 0)) (00pcStack (ListLiteral ((Num 1.2345)))) (a (Num 0))
       (aStack (ListLiteral ((Num 1.2345)))))))
    |}];
  output |> Desmos_output.to_pastable_javascript |> print_endline;
  [%expect
    {| Calc.setExpressions([{latex: "\\left\\{R_{00pc} = 0: R_{a}\\to \\left\\{1 > 1: 1, 1 > 1: 2, 3\\right\\}, R_{00pc} = 1: R_{00pc}\\to -1, R_{00pc}\\to R_{00pc}\\right\\}"}, {latex: "R_{00pc}=0"}, {latex: "R_{00pcStack}=\\left[1.2345\\right]"}, {latex: "R_{a}=0"}, {latex: "R_{aStack}=\\left[1.2345\\right]"}]) |}]
