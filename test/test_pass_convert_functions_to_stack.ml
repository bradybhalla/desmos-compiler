open! Core
open Desmos_compiler
open Languages

let compile str =
  str |> Utils.read_from_str |> Cumulative_passes.convert_functions_to_stack
  |> ok_exn |> Register_stack_instrs.sexp_of_t |> print_s

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
      (((label explicate_control_main_0)
        (body
         ((GeneralizedSet
           ((00link Push) (1local_y_0 (Set (Register y)))
            (1local_z_1 (Set (Register z)))))
          (JumpLink explicate_control_function_entry_1)
          (GeneralizedSet ((00link Pop)))))
        (control_flow Exit))
       ((label explicate_control_function_entry_1)
        (body
         ((GeneralizedSet
           ((00link Push) (1local_y_0 (PushAndSet (Register 1local_z_1)))
            (1local_z_1 (PushAndSet (Register 1local_y_0)))))
          (JumpLink explicate_control_function_entry_1)
          (GeneralizedSet ((00link Pop) (1local_y_0 Pop) (1local_z_1 Pop)))))
        (control_flow (Return (Register 1local_z_1))))))
     (registers (00link 00ret 1local_y_0 1local_z_1 y z)))
    |}]
