open! Core
open Desmos_compiler
open Languages

let compile str =
  str |> Utils.read_from_str |> Cumulative_passes.compress_instructions
  |> ok_exn |> Register_stack_instrs.sexp_of_t |> print_s

let%expect_test "simple variable assignment" =
  compile
    {|
  (decl x)
  (decl y)
  (decl z)
  (set x 5)
  (set y 5)
  (set z 5)
  |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_main_0)
        (body
         ((GeneralizedSet
           ((x (Set (Num 5))) (y (Set (Num 5))) (z (Set (Num 5)))))))
        (control_flow Exit))))
     (registers (00ret x y z)) (desmos_vars ()) (desmos_plots ()))
    |}]

let%expect_test "set z at the beginning" =
  compile
    {|
  (decl x)
  (decl y)
  (decl z)
  (set x 5)
  (set y (x + 2))
  (set z 5)
  |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_main_0)
        (body
         ((GeneralizedSet ((x (Set (Num 5))) (z (Set (Num 5)))))
          (GeneralizedSet ((y (Set (Add (Register x) (Num 2))))))))
        (control_flow Exit))))
     (registers (00ret x y z)) (desmos_vars ()) (desmos_plots ()))
    |}]

let%expect_test "set y/z together" =
  compile
    {|
  (decl x)
  (decl y)
  (decl z)
  (set x 5)
  (set y x)
  (set z x)
  |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_main_0)
        (body
         ((GeneralizedSet ((x (Set (Num 5)))))
          (GeneralizedSet ((y (Set (Register x))) (z (Set (Register x)))))))
        (control_flow Exit))))
     (registers (00ret x y z)) (desmos_vars ()) (desmos_plots ()))
    |}]

let%expect_test "set variable multiple times" =
  compile
    {|
  (decl x)
  (decl y)
  (set x 1)
  (set y 1)
  (set x (x + x))
  (set y (y + y))
  (set x (x + x))
  (set y (y + y))
  (set x (x + x))
  (set y (y + y))
  |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_main_0)
        (body
         ((GeneralizedSet ((x (Set (Num 1))) (y (Set (Num 1)))))
          (GeneralizedSet
           ((x (Set (Add (Register x) (Register x))))
            (y (Set (Add (Register y) (Register y))))))
          (GeneralizedSet
           ((x (Set (Add (Register x) (Register x))))
            (y (Set (Add (Register y) (Register y))))))
          (GeneralizedSet
           ((x (Set (Add (Register x) (Register x))))
            (y (Set (Add (Register y) (Register y))))))))
        (control_flow Exit))))
     (registers (00ret x y)) (desmos_vars ()) (desmos_plots ()))
    |}]

let%expect_test "function call" =
  compile {|
  (def f (x) (
    (return (x + 1))
  ))
  (f 3) |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_main_0)
        (body ((GeneralizedSet ((1local_x_0 (Set (Num 3)))))))
        (control_flow
         (JumpLink (target explicate_control_f_entry_1)
          (return_label convert_funcs_to_stack_0))))
       ((label convert_funcs_to_stack_0) (body ((GeneralizedSet ())))
        (control_flow Exit))
       ((label explicate_control_f_entry_1) (body ())
        (control_flow (Return (Add (Register 1local_x_0) (Num 1)))))))
     (registers (00ret 1local_x_0)) (desmos_vars ()) (desmos_plots ()))
    |}]
