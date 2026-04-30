open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "if" =
  let prog : C_style_separated_functions.t =
    let open C_style_separated_functions in
    let x = Register.of_string "x" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          If { branches = [ (Bool true, [ Set (x, Num 1.) ]) ]; else_ = [] };
          Set (x, Num 1.);
        ];
    }
  in
  prog |> Pass_explicate_control.compile |> Register_func_instrs.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      (((label label_0_main) (body ())
        (control_flow
         (Jump (conds (((Bool true) label_2_if_statement_branch)))
          (default label_1_if_statement_exit))))
       ((label label_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_1_if_statement_exit) (body ((Set x (Num 1))))
        (control_flow Exit)))))
    |}]

let%expect_test "if / elif" =
  let prog : C_style_separated_functions.t =
    let open C_style_separated_functions in
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
    }
  in
  prog |> Pass_explicate_control.compile |> Register_func_instrs.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      (((label label_0_main) (body ())
        (control_flow
         (Jump
          (conds
           (((Bool true) label_2_if_statement_branch)
            ((Bool false) label_3_if_statement_branch)))
          (default label_1_if_statement_exit))))
       ((label label_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_3_if_statement_branch) (body ((Set x (Num 2))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_1_if_statement_exit) (body ((Set x (Num 1))))
        (control_flow Exit)))))
    |}]

let%expect_test "if / else" =
  let prog : C_style_separated_functions.t =
    let open C_style_separated_functions in
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
    }
  in
  prog |> Pass_explicate_control.compile |> Register_func_instrs.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      (((label label_0_main) (body ())
        (control_flow
         (Jump (conds (((Bool true) label_2_if_statement_branch)))
          (default label_3_if_statement_else))))
       ((label label_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_3_if_statement_else) (body ((Set x (Num 2))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_1_if_statement_exit) (body ((Set x (Num 1))))
        (control_flow Exit)))))
    |}]

let%expect_test "if / elif / else" =
  let prog : C_style_separated_functions.t =
    let open C_style_separated_functions in
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
    }
  in
  prog |> Pass_explicate_control.compile |> Register_func_instrs.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      (((label label_0_main) (body ())
        (control_flow
         (Jump
          (conds
           (((Bool true) label_2_if_statement_branch)
            ((Bool false) label_3_if_statement_branch)))
          (default label_4_if_statement_else))))
       ((label label_2_if_statement_branch) (body ((Set x (Num 1))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_3_if_statement_branch) (body ((Set x (Num 2))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_4_if_statement_else) (body ((Set x (Num 3))))
        (control_flow (Jump (conds ()) (default label_1_if_statement_exit))))
       ((label label_1_if_statement_exit) (body ()) (control_flow Exit)))))
    |}]

let%expect_test "simple while (no statements to prepare condition)" =
  let prog : C_style_separated_functions.t =
    let open C_style_separated_functions in
    let x = Register.of_string "x" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          While
            {
              cond = ([], Compare (Lt, Register x, Num 10.));
              body = [ Set (x, Add (Register x, Num 1.)) ];
            };
          Set (x, Num 1.);
        ];
    }
  in
  prog |> Pass_explicate_control.compile |> Register_func_instrs.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      (((label label_0_main) (body ())
        (control_flow
         (Jump (conds (((Compare Lt (Register x) (Num 10)) label_2_while_entry)))
          (default label_1_while_end))))
       ((label label_2_while_entry) (body ((Set x (Add (Register x) (Num 1)))))
        (control_flow
         (Jump (conds (((Compare Lt (Register x) (Num 10)) label_2_while_entry)))
          (default label_1_while_end))))
       ((label label_1_while_end) (body ((Set x (Num 1)))) (control_flow Exit)))))
    |}]

let%expect_test "while with statements needed to prepare condition" =
  let prog : C_style_separated_functions.t =
    let open C_style_separated_functions in
    let x = Register.of_string "x" in
    let f = Function_name.of_string "f" in
    {
      functions = Function_name.Map.of_alist_exn [];
      main =
        [
          While
            {
              cond =
                ( [ Call { func_name = f; args = []; ret = Some x } ],
                  Compare (Lt, Register x, Num 10.) );
              body = [ Set (x, Add (Register x, Num 1.)) ];
            };
          Set (x, Num 1.);
        ];
    }
  in
  prog |> Pass_explicate_control.compile |> Register_func_instrs.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      (((label label_0_main) (body ((Call (func_name f) (args ()) (ret (x)))))
        (control_flow
         (Jump (conds (((Compare Lt (Register x) (Num 10)) label_2_while_entry)))
          (default label_1_while_end))))
       ((label label_2_while_entry)
        (body
         ((Set x (Add (Register x) (Num 1)))
          (Call (func_name f) (args ()) (ret (x)))))
        (control_flow
         (Jump (conds (((Compare Lt (Register x) (Num 10)) label_2_while_entry)))
          (default label_1_while_end))))
       ((label label_1_while_end) (body ((Set x (Num 1)))) (control_flow Exit)))))
    |}]
