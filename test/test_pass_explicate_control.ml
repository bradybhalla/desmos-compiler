open! Core
open! Desmos_compiler
open Languages
open Types

let%expect_test "if" =
  let prog : C_style_registers.t =
    let open C_style_separated_functions in
    let open C_style_registers in
    let x = Register.of_string "x" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          If { branches = [ (Bool true, [ Set (x, Num 1.) ]) ]; else_ = [] };
          Set (x, Num 1.);
        ];
      registers = Register.Set.empty;
    }
  in
  prog |> Passes.explicate_control |> Register_func_instrs.sexp_of_t |> print_s;
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
     (registers ()))
    |}]

let%expect_test "if / elif" =
  let prog : C_style_registers.t =
    let open C_style_separated_functions in
    let open C_style_registers in
    let x = Register.of_string "x" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          If
            {
              branches =
                [
                  (Bool true, [ Set (x, Num 1.) ]);
                  (Bool false, [ Set (x, Num 2.) ]);
                ];
              else_ = [];
            };
          Set (x, Num 1.);
        ];
      registers = Register.Set.empty;
    }
  in
  prog |> Passes.explicate_control |> Register_func_instrs.sexp_of_t |> print_s;
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
     (registers ()))
    |}]

let%expect_test "if / else" =
  let prog : C_style_registers.t =
    let open C_style_separated_functions in
    let open C_style_registers in
    let x = Register.of_string "x" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          If
            {
              branches = [ (Bool true, [ Set (x, Num 1.) ]) ];
              else_ = [ Set (x, Num 2.) ];
            };
          Set (x, Num 1.);
        ];
      registers = Register.Set.empty;
    }
  in
  prog |> Passes.explicate_control |> Register_func_instrs.sexp_of_t |> print_s;
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
     (registers ()))
    |}]

let%expect_test "if / elif / else" =
  let prog : C_style_registers.t =
    let open C_style_separated_functions in
    let open C_style_registers in
    let x = Register.of_string "x" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          If
            {
              branches =
                [
                  (Bool true, [ Set (x, Num 1.) ]);
                  (Bool false, [ Set (x, Num 2.) ]);
                ];
              else_ = [ Set (x, Num 3.) ];
            };
        ];
      registers = Register.Set.empty;
    }
  in
  prog |> Passes.explicate_control |> Register_func_instrs.sexp_of_t |> print_s;
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
     (registers ()))
    |}]

let%expect_test "function where all branches return (nested if)" =
  (* the current behavior is that this creates some empty unreachable blocks *)
  let prog : C_style_registers.t =
    let open C_style_separated_functions in
    let open C_style_registers in
    let x = Register.of_string "x" in
    let f = Function_name.of_string "f" in
    {
      functions =
        Function_name.Map.of_alist_exn
          [
            ( f,
              {
                params = [ x ];
                body =
                  [
                    If
                      {
                        branches = [ (Bool true, [ Return (Num 1.) ]) ];
                        else_ =
                          [
                            If
                              {
                                branches = [ (Bool false, [ Return (Num 2.) ]) ];
                                else_ = [ Return (Num 0.) ];
                              };
                          ];
                      };
                  ];
              } );
          ];
      main = [];
      registers = Register.Set.empty;
    }
  in
  prog |> Passes.explicate_control |> Register_func_instrs.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions
      ((f
        ((entry_label explicate_control_1_function_entry) (params (x))
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
            (control_flow Exit))))))))
     (main (((label explicate_control_0_main) (body ()) (control_flow Exit))))
     (registers ()))
    |}]

let%expect_test "while loop" =
  let prog : C_style_registers.t =
    let open C_style_separated_functions in
    let open C_style_registers in
    let x = Register.of_string "x" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          While
            {
              cond = Compare (Lt, Register x, Num 10.);
              body = [ Set (x, Add (Register x, Num 1.)) ];
            };
          Set (x, Num 1.);
        ];
      registers = Register.Set.empty;
    }
  in
  prog |> Passes.explicate_control |> Register_func_instrs.sexp_of_t |> print_s;
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
     (registers ()))
    |}]
