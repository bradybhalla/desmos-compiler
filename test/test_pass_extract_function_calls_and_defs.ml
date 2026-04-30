open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "toplevel function defs extracted correctly" =
  let prog : C_style_frontend.t =
    let open C_style_frontend in
    let f = Function_name.of_string "f" in
    let x = Register.of_string "x" in
    let y = Register.of_string "y" in
    [ Function_def (f, [ x ], [ Return (Register x) ]); Set (y, Num 5.) ]
  in
  prog |> Pass_extract_function_calls_and_defs.compile
  |> C_style_separated_functions.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions ((f ((params (x)) (body ((Return (Register x))))))))
     (main ((Set y (Num 5)))))
    |}]

let%expect_test "nested function calls extracted into separate statements" =
  let prog : C_style_frontend.t =
    let open C_style_frontend in
    let f = Function_name.of_string "f" in
    let g = Function_name.of_string "g" in
    let h = Function_name.of_string "h" in
    let x = Register.of_string "x" in
    [ Call (f, [ Call (g, [ Call (h, [ Register x ]) ]) ]) ]
  in
  prog |> Pass_extract_function_calls_and_defs.compile
  |> C_style_separated_functions.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name h) (args ((Register x)))
        (ret (0extract_function_calls_0)))
       (Call (func_name g) (args ((Register 0extract_function_calls_0)))
        (ret (0extract_function_calls_1)))
       (Call (func_name f) (args ((Register 0extract_function_calls_1)))
        (ret ())))))
    |}]

let%expect_test "function call extracted from binary expression" =
  let prog : C_style_frontend.t =
    let open C_style_frontend in
    let f = Function_name.of_string "f" in
    let x = Register.of_string "x" in
    let y = Register.of_string "y" in
    [ Set (y, Add (Num 1., Call (f, [ Register x ]))) ]
  in
  prog |> Pass_extract_function_calls_and_defs.compile
  |> C_style_separated_functions.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Register x)))
        (ret (0extract_function_calls_0)))
       (Set y (Add (Num 1) (Register 0extract_function_calls_0))))))
    |}]

let%expect_test "nested binary expressions extract function calls left to right"
    =
  let prog : C_style_frontend.t =
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
  in
  prog |> Pass_extract_function_calls_and_defs.compile
  |> C_style_separated_functions.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Register x)))
        (ret (0extract_function_calls_0)))
       (Call (func_name f) (args ((Register y)))
        (ret (0extract_function_calls_1)))
       (Call (func_name f) (args ((Register z)))
        (ret (0extract_function_calls_2)))
       (Call (func_name f) (args ((Register a)))
        (ret (0extract_function_calls_3)))
       (Call (func_name f) (args ((Register b)))
        (ret (0extract_function_calls_4)))
       (Set result
        (Add (Register 0extract_function_calls_0)
         (Div
          (Sub
           (Add (Register 0extract_function_calls_1)
            (Register 0extract_function_calls_2))
           (Register 0extract_function_calls_3))
          (Register 0extract_function_calls_4)))))))
    |}]

let%expect_test "function call extracted from while condition" =
  let prog : C_style_frontend.t =
    let open C_style_frontend in
    let f = Function_name.of_string "f" in
    let x = Register.of_string "x" in
    [ While (Call (f, [ Register x ]), []) ]
  in
  prog |> Pass_extract_function_calls_and_defs.compile
  |> C_style_separated_functions.sexp_of_t |> print_s;
  [%expect {| |}]

let%expect_test "function call extracted from first if condition" =
  let prog : C_style_frontend.t =
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
  in
  prog |> Pass_extract_function_calls_and_defs.compile
  |> C_style_separated_functions.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions ())
     (main
      ((Call (func_name f) (args ((Register x)))
        (ret (0extract_function_calls_0)))
       (If (branches (((Register 0extract_function_calls_0) ((Set y (Num 1))))))
        (else_ ())))))
    |}]
