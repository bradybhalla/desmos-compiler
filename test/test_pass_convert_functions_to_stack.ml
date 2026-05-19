open! Core
open Desmos_compiler
open Languages

let compile str =
  str |> Utils.read_from_str |> Cumulative_passes.convert_functions_to_stack
  |> Register_stack_instrs.sexp_of_t |> print_s

let%expect_test "register saving and restoring at correct call sites" =
  (* we should see that a function call outside of a function doesn't use
     the stack but a call inside does  *)
  compile
    {|
  (decl y)
  (decl z)
  (def f (y z) (
    (f z y)
    (return z)
  ))
  (f y z) |};
  [%expect
    {|
    ((blocks
      (((label explicate_control_0_main)
        (body
         ((GeneralizedSet
           ((00link Push) (1rename_local_vars_0 (Set (Register y)))
            (1rename_local_vars_1 (Set (Register z)))))
          (JumpLink explicate_control_1_function_entry)
          (GeneralizedSet ((00link Pop)))))
        (control_flow Exit))
       ((label explicate_control_1_function_entry)
        (body
         ((GeneralizedSet
           ((00link Push)
            (1rename_local_vars_0 (PushAndSet (Register 1rename_local_vars_1)))
            (1rename_local_vars_1 (PushAndSet (Register 1rename_local_vars_0)))))
          (JumpLink explicate_control_1_function_entry)
          (GeneralizedSet
           ((00link Pop) (1rename_local_vars_0 Pop) (1rename_local_vars_1 Pop)))))
        (control_flow (Return (Register 1rename_local_vars_1))))))
     (registers (00link 00ret 1rename_local_vars_0 1rename_local_vars_1 y z)))
    |}]
