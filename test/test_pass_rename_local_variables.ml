open! Core
open! Desmos_compiler
open! Languages
open! Types

let compile str =
  str |> Utils.read_from_str |> Pass_check_function_defs.compile |> ok_exn
  |> Pass_extract_function_calls_and_defs.compile
  |> Pass_check_variable_scopes.compile |> ok_exn
  |> Pass_rename_local_variables.compile

let%expect_test "rename local variables" =
  compile
    {|
    (decl x)
    (decl y)
    (set x 0)
    (set y 0)

    (def foo (a b) (
      (decl z)
      (set z (+ a b))
      (if (z > 0) (
        (decl p)
        (set p z)
        (return p)
      ) else (
        (while (b > 0) (
          (decl q)
          (set q b)
          (set b (- b 1))
        ))
        (return z)
      ))
    ))

    (if (x > 0) (
      (decl r)
      (set r x)
      (set y r)
    ) else (
      (while (y > 0) (
        (decl s)
        (set s y)
        (set y (- s 1))
      ))
    ))
    |}
  |> [%sexp_of: C_style_registers.t] |> print_s;
  [%expect
    {|
    ((functions
      ((foo
        ((params (1rename_local_vars_3 1rename_local_vars_4))
         (body
          ((Call (func_name +)
            (args
             ((Register 1rename_local_vars_3) (Register 1rename_local_vars_4)))
            (ret (1rename_local_vars_6)))
           (Set 1rename_local_vars_5 (Register 1rename_local_vars_6))
           (If
            (branches
             (((Compare Gt (Register 1rename_local_vars_5) (Num 0))
               ((Set 1rename_local_vars_9 (Register 1rename_local_vars_5))
                (Return (Register 1rename_local_vars_9))))))
            (else_
             ((While (cond (Compare Gt (Register 1rename_local_vars_4) (Num 0)))
               (body
                ((Set 1rename_local_vars_7 (Register 1rename_local_vars_4))
                 (Call (func_name -)
                  (args ((Register 1rename_local_vars_4) (Num 1)))
                  (ret (1rename_local_vars_8)))
                 (Set 1rename_local_vars_4 (Register 1rename_local_vars_8)))))
              (Return (Register 1rename_local_vars_5)))))))))))
     (main
      ((Set x (Num 0)) (Set y (Num 0))
       (If
        (branches
         (((Compare Gt (Register x) (Num 0))
           ((Set 1rename_local_vars_2 (Register x))
            (Set y (Register 1rename_local_vars_2))))))
        (else_
         ((While (cond (Compare Gt (Register y) (Num 0)))
           (body
            ((Set 1rename_local_vars_0 (Register y))
             (Call (func_name -) (args ((Register 1rename_local_vars_0) (Num 1)))
              (ret (1rename_local_vars_1)))
             (Set y (Register 1rename_local_vars_1))))))))))
     (registers
      (1rename_local_vars_0 1rename_local_vars_1 1rename_local_vars_2 x y)))
    |}]
