open! Core
open Desmos_compiler
open Languages

let compile prog =
  prog |> Utils.read_from_str
  |> Cumulative_passes.extract_functions_and_plotting |> ok_exn
  |> [%sexp_of: [ `Unchecked ] C_style_separated_functions.t] |> print_s

let%expect_test "toplevel function defs extracted correctly" =
  compile {|
  (def f (x) (
    (return x)
  ))
  (set y 5) |};
  [%expect
    {|
    ((functions ((f ((params (x)) (body ((Return (Register x))))))))
     (main ((Set y (Num 5)))) (status Unchecked) (desmos_decls ())
     (desmos_plot ()))
    |}]

let%expect_test "nested function calls extracted into separate statements" =
  compile {| (f (g (h x))) |};
  [%expect
    {|
    ((functions ())
     (main
      ((Decl 1extract_call_0)
       (Call (func_name h) (args ((Register x))) (ret (1extract_call_0)))
       (Decl 1extract_call_1)
       (Call (func_name g) (args ((Register 1extract_call_0)))
        (ret (1extract_call_1)))
       (Call (func_name f) (args ((Register 1extract_call_1))) (ret ()))))
     (status Unchecked) (desmos_decls ()) (desmos_plot ()))
    |}]

let%expect_test "nested binary expressions extract function calls left to right"
    =
  compile {|
  (set result (((f x) + (((f y) + (f z)) - (f a))) / (f b))) |};
  [%expect
    {|
    ((functions ())
     (main
      ((Decl 1extract_call_0)
       (Call (func_name f) (args ((Register x))) (ret (1extract_call_0)))
       (Decl 1extract_call_1)
       (Call (func_name f) (args ((Register y))) (ret (1extract_call_1)))
       (Decl 1extract_call_2)
       (Call (func_name f) (args ((Register z))) (ret (1extract_call_2)))
       (Decl 1extract_call_3)
       (Call (func_name f) (args ((Register a))) (ret (1extract_call_3)))
       (Decl 1extract_call_4)
       (Call (func_name f) (args ((Register b))) (ret (1extract_call_4)))
       (Set result
        (Div
         (Add (Register 1extract_call_0)
          (Sub (Add (Register 1extract_call_1) (Register 1extract_call_2))
           (Register 1extract_call_3)))
         (Register 1extract_call_4)))))
     (status Unchecked) (desmos_decls ()) (desmos_plot ()))
    |}]

let%expect_test "function call extracted from while condition" =
  compile {| (while (f x) ()) |};
  [%expect
    {|
    ((functions ())
     (main
      ((Decl 1extract_call_0)
       (Call (func_name f) (args ((Register x))) (ret (1extract_call_0)))
       (While (cond (Register 1extract_call_0))
        (body
         ((Call (func_name f) (args ((Register x))) (ret (1extract_call_0))))))))
     (status Unchecked) (desmos_decls ()) (desmos_plot ()))
    |}]

let%expect_test "function call extracted from first if condition" =
  compile {|
  (if (f x) (
    (set y 1)
  )) |};
  [%expect
    {|
    ((functions ())
     (main
      ((Decl 1extract_call_0)
       (Call (func_name f) (args ((Register x))) (ret (1extract_call_0)))
       (If (branches (((Register 1extract_call_0) ((Set y (Num 1))))))
        (else_ ()))))
     (status Unchecked) (desmos_decls ()) (desmos_plot ()))
    |}]

let%expect_test "function call extracted from many if conditions" =
  compile
    {|
  (if (f x) (
    (set y 1)
  ) elif (f y) (
    (set y 2)
  ) elif x (
    (set y 3)
  ) elif (f 1) (
    (set y 4)
  ) else (
    (set y 5)
  )) |};
  [%expect
    {|
    ((functions ())
     (main
      ((Decl 1extract_call_0)
       (Call (func_name f) (args ((Register x))) (ret (1extract_call_0)))
       (If (branches (((Register 1extract_call_0) ((Set y (Num 1))))))
        (else_
         ((Decl 1extract_call_1)
          (Call (func_name f) (args ((Register y))) (ret (1extract_call_1)))
          (If
           (branches
            (((Register 1extract_call_1) ((Set y (Num 2))))
             ((Register x) ((Set y (Num 3))))))
           (else_
            ((Decl 1extract_call_2)
             (Call (func_name f) (args ((Num 1))) (ret (1extract_call_2)))
             (If (branches (((Register 1extract_call_2) ((Set y (Num 4))))))
              (else_ ((Set y (Num 5)))))))))))))
     (status Unchecked) (desmos_decls ()) (desmos_plot ()))
    |}]

let%expect_test "function call in And/Or respects short circuiting" =
  compile
    {|
  (set y ((f 1) && (f 1)))
  (set y ((f 2) || (f 2)))
  (set y ((f 1) && ((f 1) || (f 3))))
  (set y (x || (y && (f 4)))) |};
  [%expect
    {|
    ((functions ())
     (main
      ((Decl 1extract_and_2) (Decl 1extract_call_0)
       (Call (func_name f) (args ((Num 1))) (ret (1extract_call_0)))
       (If
        (branches
         (((Register 1extract_call_0)
           ((Decl 1extract_call_1)
            (Call (func_name f) (args ((Num 1))) (ret (1extract_call_1)))
            (Set 1extract_and_2 (Register 1extract_call_1))))))
        (else_ ((Set 1extract_and_2 (Bool false)))))
       (Set y (Register 1extract_and_2)) (Decl 1extract_or_5)
       (Decl 1extract_call_3)
       (Call (func_name f) (args ((Num 2))) (ret (1extract_call_3)))
       (If
        (branches
         (((Register 1extract_call_3) ((Set 1extract_or_5 (Bool true))))))
        (else_
         ((Decl 1extract_call_4)
          (Call (func_name f) (args ((Num 2))) (ret (1extract_call_4)))
          (Set 1extract_or_5 (Register 1extract_call_4)))))
       (Set y (Register 1extract_or_5)) (Decl 1extract_and_10)
       (Decl 1extract_call_6)
       (Call (func_name f) (args ((Num 1))) (ret (1extract_call_6)))
       (If
        (branches
         (((Register 1extract_call_6)
           ((Decl 1extract_or_9) (Decl 1extract_call_7)
            (Call (func_name f) (args ((Num 1))) (ret (1extract_call_7)))
            (If
             (branches
              (((Register 1extract_call_7) ((Set 1extract_or_9 (Bool true))))))
             (else_
              ((Decl 1extract_call_8)
               (Call (func_name f) (args ((Num 3))) (ret (1extract_call_8)))
               (Set 1extract_or_9 (Register 1extract_call_8)))))
            (Set 1extract_and_10 (Register 1extract_or_9))))))
        (else_ ((Set 1extract_and_10 (Bool false)))))
       (Set y (Register 1extract_and_10)) (Decl 1extract_or_13)
       (If (branches (((Register x) ((Set 1extract_or_13 (Bool true))))))
        (else_
         ((Decl 1extract_and_12)
          (If
           (branches
            (((Register y)
              ((Decl 1extract_call_11)
               (Call (func_name f) (args ((Num 4))) (ret (1extract_call_11)))
               (Set 1extract_and_12 (Register 1extract_call_11))))))
           (else_ ((Set 1extract_and_12 (Bool false)))))
          (Set 1extract_or_13 (Register 1extract_and_12)))))
       (Set y (Register 1extract_or_13))))
     (status Unchecked) (desmos_decls ()) (desmos_plot ()))
    |}]
