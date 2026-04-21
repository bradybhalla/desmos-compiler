open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "register saving and restoring at a call site" =
  let prog : Register_func_instrs_with_call_liveness.t =
    let open Register_func_instrs_with_call_liveness in
    let f = Function_name.of_string "f" in
    let f_label = Label.of_string "f" in
    let x = Register.of_string "x" in
    let y = Register.of_string "y" in
    let z = Register.of_string "z" in
    {
      functions =
        Function_name.Map.of_alist_exn
          [ (f, { entry_label = f_label; params = [ y; z ]; blocks = [] }) ];
      main =
        [
          {
            label = Label.of_string "main";
            body =
              [
                Call
                  {
                    func_name = f;
                    args = [ Register y; Register x ];
                    ret = Some (Register.of_string "a");
                    live_registers = [ x; y; z ];
                  };
              ];
            control_flow = Exit;
          };
        ];
    }
  in
  prog |> Pass_convert_functions_to_stack.compile
  |> Register_stack_instrs.sexp_of_t |> print_s;
  [%expect
    {|
    (((label main)
      (body
       ((GeneralizedSet
         ((00link Push) (y (PushAndSet (Register y)))
          (z (PushAndSet (Register x))) (x Push)))
        (JumpLink f) (GeneralizedSet ((00link Pop) (x Pop) (y Pop) (z Pop)))
        (GeneralizedSet ((a (Set (Register 00ret)))))))
      (control_flow Exit)))
    |}]
