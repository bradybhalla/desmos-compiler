open! Core
open Desmos_compiler
open Languages

let compile str =
  str |> Utils.read_from_str |> Cumulative_passes.compress_instructions
  |> ok_exn |> Register_stack_instrs.sexp_of_t |> print_s

let%expect_test "simple variable assignment" =
  compile {|
  (decl x)
  (set x 5) |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_main_0)
        (body ((GeneralizedSet ((x (Set (Num 5))))))) (control_flow Exit))))
     (registers (00ret x)))
    |}]

let%expect_test "function call" =
  compile
    {|
  (def f (x) (
    (return (x + 1))
  ))
  (decl y)
  (set y (f 3)) |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_main_0)
        (body ((GeneralizedSet ((1local_x_0 (Set (Num 3)))))))
        (control_flow
         (JumpLink (target explicate_control_function_entry_1)
          (return_label convert_funcs_to_stack_0))))
       ((label convert_funcs_to_stack_0)
        (body
         ((GeneralizedSet ())
          (GeneralizedSet ((1extract_call_0 (Set (Register 00ret)))))
          (GeneralizedSet ((y (Set (Register 1extract_call_0)))))))
        (control_flow Exit))
       ((label explicate_control_function_entry_1) (body ())
        (control_flow (Return (Add (Register 1local_x_0) (Num 1)))))))
     (registers (00ret 1extract_call_0 1local_x_0 y)))
    |}]
