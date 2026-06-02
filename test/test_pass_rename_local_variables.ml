open! Core
open Desmos_compiler
open Languages

let compile str =
  str |> Utils.read_from_str |> Cumulative_passes.rename_local_variables
  |> ok_exn

let%expect_test "rename local variables" =
  compile
    {|
    (decl x)
    (decl y)
    (set x 0)
    (set y 0)

    (def foo (a b) (
      (decl z)
      (set z (a + b))
      (if (z > 0) (
        (decl p)
        (set p z)
        (return p)
      ) else (
        (while (b > 0) (
          (decl q)
          (set q b)
          (set b (b - 1))
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
        (set y (s - 1))
      ))
    ))
    |}
  |> [%sexp_of: C_style_registers.t] |> print_s;
  [%expect
    {|
    ((functions
      ((foo
        ((params (1local_a_2 1local_b_3))
         (body
          ((Set 1local_z_4 (Add (Register 1local_a_2) (Register 1local_b_3)))
           (If
            (branches
             (((Compare Gt (Register 1local_z_4) (Num 0))
               ((Set 1local_p_6 (Register 1local_z_4))
                (Return (Register 1local_p_6))))))
            (else_
             ((While (cond (Compare Gt (Register 1local_b_3) (Num 0)))
               (body
                ((Set 1local_q_5 (Register 1local_b_3))
                 (Set 1local_b_3 (Sub (Register 1local_b_3) (Num 1))))))
              (Return (Register 1local_z_4)))))))
         (local_registers
          (1local_a_2 1local_b_3 1local_p_6 1local_q_5 1local_z_4))))))
     (main
      ((Set x (Num 0)) (Set y (Num 0))
       (If
        (branches
         (((Compare Gt (Register x) (Num 0))
           ((Set 1local_r_1 (Register x)) (Set y (Register 1local_r_1))))))
        (else_
         ((While (cond (Compare Gt (Register y) (Num 0)))
           (body
            ((Set 1local_s_0 (Register y))
             (Set y (Sub (Register 1local_s_0) (Num 1)))))))))))
     (global_registers (1local_r_1 1local_s_0 x y)) (desmos_vars ())
     (desmos_plots ()))
    |}]
