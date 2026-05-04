open! Core
open! Desmos_compiler
open! Languages
open! Types

let print_result stmts =
  { stmts; info = `Unchecked }
  |> Check_function_defs.check |> ok_exn
  |> Pass_extract_function_calls_and_defs.compile
  |> C_style_separated_functions.sexp_of_t |> print_s

let%expect_test "toplevel function defs extracted correctly" =
  let open C_style_frontend in
  let f = Function_name.of_string "f" in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  [ Function_def (f, [ x ], [ Return (Register x) ]); Set (y, Num 5.) ]
  |> print_result;
  [%expect
    {|
    ((functions ((f ((params (x)) (body ((Return (Register x))))))))
     (main ((Set y (Num 5)))))
    |}]

let%expect_test "nested function calls extracted into separate statements" =
  let open C_style_frontend in
  let f = Function_name.of_string "f" in
  let g = Function_name.of_string "g" in
  let h = Function_name.of_string "h" in
  let x = Register.of_string "x" in
  [ Call (f, [ Call (g, [ Call (h, [ Register x ]) ]) ]) ] |> print_result;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name h) (args ((Register x)))
        (ret (0extract_function_calls_0_call)))
       (Call (func_name g) (args ((Register 0extract_function_calls_0_call)))
        (ret (0extract_function_calls_1_call)))
       (Call (func_name f) (args ((Register 0extract_function_calls_1_call)))
        (ret ())))))
    |}]

let%expect_test "nested binary expressions extract function calls left to right"
    =
  let open C_style_frontend in
  let f = Function_name.of_string "f" in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  let z = Register.of_string "z" in
  let a = Register.of_string "a" in
  let b = Register.of_string "b" in
  let result = Register.of_string "result" in
  (* f(x) + (f(y) + f(z) - f(a)) / f(b) *)
  [
    Set
      ( result,
        Add
          ( Call (f, [ Register x ]),
            Div
              ( Sub
                  ( Add (Call (f, [ Register y ]), Call (f, [ Register z ])),
                    Call (f, [ Register a ]) ),
                Call (f, [ Register b ]) ) ) );
  ]
  |> print_result;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Register x)))
        (ret (0extract_function_calls_0_call)))
       (Call (func_name f) (args ((Register y)))
        (ret (0extract_function_calls_1_call)))
       (Call (func_name f) (args ((Register z)))
        (ret (0extract_function_calls_2_call)))
       (Call (func_name f) (args ((Register a)))
        (ret (0extract_function_calls_3_call)))
       (Call (func_name f) (args ((Register b)))
        (ret (0extract_function_calls_4_call)))
       (Set result
        (Add (Register 0extract_function_calls_0_call)
         (Div
          (Sub
           (Add (Register 0extract_function_calls_1_call)
            (Register 0extract_function_calls_2_call))
           (Register 0extract_function_calls_3_call))
          (Register 0extract_function_calls_4_call)))))))
    |}]

let%expect_test "function call extracted from while condition" =
  let open C_style_frontend in
  let f = Function_name.of_string "f" in
  let x = Register.of_string "x" in
  [ While (Call (f, [ Register x ]), []) ] |> print_result;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Register x)))
        (ret (0extract_function_calls_0_call)))
       (While (cond (Register 0extract_function_calls_0_call))
        (body
         ((Call (func_name f) (args ((Register x)))
           (ret (0extract_function_calls_0_call)))))))))
    |}]

let%expect_test "function call extracted from first if condition" =
  let open C_style_frontend in
  let f = Function_name.of_string "f" in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  [
    If
      {
        branches = [ (Call (f, [ Register x ]), [ Set (y, Num 1.) ]) ];
        else_ = [];
      };
  ]
  |> print_result;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Register x)))
        (ret (0extract_function_calls_0_call)))
       (If
        (branches
         (((Register 0extract_function_calls_0_call) ((Set y (Num 1))))))
        (else_ ())))))
    |}]

let%expect_test "function call extracted from many if conditions" =
  let open C_style_frontend in
  let f = Function_name.of_string "f" in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  [
    If
      {
        branches =
          [
            (Call (f, [ Register x ]), [ Set (y, Num 1.) ]);
            (Call (f, [ Register y ]), [ Set (y, Num 2.) ]);
            (* don't extract from this one*)
            (Register x, [ Set (y, Num 3.) ]);
            (Call (f, [ Num 1. ]), [ Set (y, Num 4.) ]);
          ];
        else_ = [ Set (y, Num 5.) ];
      };
  ]
  |> print_result;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Register x)))
        (ret (0extract_function_calls_0_call)))
       (If
        (branches
         (((Register 0extract_function_calls_0_call) ((Set y (Num 1))))))
        (else_
         ((Call (func_name f) (args ((Register y)))
           (ret (0extract_function_calls_1_call)))
          (If
           (branches
            (((Register 0extract_function_calls_1_call) ((Set y (Num 2))))
             ((Register x) ((Set y (Num 3))))))
           (else_
            ((Call (func_name f) (args ((Num 1)))
              (ret (0extract_function_calls_2_call)))
             (If
              (branches
               (((Register 0extract_function_calls_2_call) ((Set y (Num 4))))))
              (else_ ((Set y (Num 5))))))))))))))
    |}]

let%expect_test "function call in And/Or respects short circuiting" =
  let open C_style_frontend in
  let f = Function_name.of_string "f" in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  [
    Set (y, And (Call (f, [ Num 1. ]), Call (f, [ Num 1. ])));
    Set (y, Or (Call (f, [ Num 2. ]), Call (f, [ Num 2. ])));
    Set
      ( y,
        And
          (Call (f, [ Num 1. ]), Or (Call (f, [ Num 1. ]), Call (f, [ Num 3. ])))
      );
    Set (y, Or (Register x, And (Register y, Call (f, [ Num 4. ]))));
  ]
  |> print_result;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Num 1)))
        (ret (0extract_function_calls_0_call)))
       (If
        (branches
         (((Register 0extract_function_calls_0_call)
           ((Call (func_name f) (args ((Num 1)))
             (ret (0extract_function_calls_1_call)))
            (Set 0extract_function_calls_2_and
             (Register 0extract_function_calls_1_call))))))
        (else_ ((Set 0extract_function_calls_2_and (Bool false)))))
       (Set y (Register 0extract_function_calls_2_and))
       (Call (func_name f) (args ((Num 2)))
        (ret (0extract_function_calls_3_call)))
       (If
        (branches
         (((Register 0extract_function_calls_3_call)
           ((Set 0extract_function_calls_5_or (Bool true))))))
        (else_
         ((Call (func_name f) (args ((Num 2)))
           (ret (0extract_function_calls_4_call)))
          (Set 0extract_function_calls_5_or
           (Register 0extract_function_calls_4_call)))))
       (Set y (Register 0extract_function_calls_5_or))
       (Call (func_name f) (args ((Num 1)))
        (ret (0extract_function_calls_6_call)))
       (If
        (branches
         (((Register 0extract_function_calls_6_call)
           ((Call (func_name f) (args ((Num 1)))
             (ret (0extract_function_calls_7_call)))
            (If
             (branches
              (((Register 0extract_function_calls_7_call)
                ((Set 0extract_function_calls_9_or (Bool true))))))
             (else_
              ((Call (func_name f) (args ((Num 3)))
                (ret (0extract_function_calls_8_call)))
               (Set 0extract_function_calls_9_or
                (Register 0extract_function_calls_8_call)))))
            (Set 0extract_function_calls_10_and
             (Register 0extract_function_calls_9_or))))))
        (else_ ((Set 0extract_function_calls_10_and (Bool false)))))
       (Set y (Register 0extract_function_calls_10_and))
       (If
        (branches
         (((Register x) ((Set 0extract_function_calls_13_or (Bool true))))))
        (else_
         ((If
           (branches
            (((Register y)
              ((Call (func_name f) (args ((Num 4)))
                (ret (0extract_function_calls_11_call)))
               (Set 0extract_function_calls_12_and
                (Register 0extract_function_calls_11_call))))))
           (else_ ((Set 0extract_function_calls_12_and (Bool false)))))
          (Set 0extract_function_calls_13_or
           (Register 0extract_function_calls_12_and)))))
       (Set y (Register 0extract_function_calls_13_or)))))
    |}]
