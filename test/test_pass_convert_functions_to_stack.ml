open! Core
open Desmos_compiler
open Languages
open Types

let%expect_test "register saving and restoring at a call site" =
  let prog : Register_func_instrs.t =
    let open Register_func_instrs in
    let f = Function_name.of_string "f" in
    let f_label = Label.of_string "f" in
    let x = Register.of_string "x" in
    let y = Register.of_string "y" in
    let z = Register.of_string "z" in
    let a = Register.of_string "a" in
    let local_y = Register.of_string "local_y" in
    let local_z = Register.of_string "local_z" in
    {
      functions =
        Function_name.Map.of_alist_exn
          [
            ( f,
              {
                entry_label = f_label;
                params = [ local_y; local_z ];
                blocks =
                  [
                    {
                      label = Label.of_string "func_entry";
                      body =
                        [
                          Call
                            {
                              func_name = f;
                              args = [ Register local_z; Register local_y ];
                              ret = Some local_z;
                            };
                        ];
                      control_flow = Exit;
                    };
                  ];
                local_registers = Register.Set.of_list [ local_y; local_z ];
              } );
          ];
      main =
        [
          {
            label = Label.of_string "main";
            body =
              [
                Call
                  {
                    func_name = f;
                    args = [ Register y; Register z ];
                    ret = Some a;
                  };
              ];
            control_flow = Exit;
          };
        ];
      global_registers = Register.Set.of_list [ x; y; a ];
    }
  in
  prog |> Passes.convert_functions_to_stack |> Register_stack_instrs.sexp_of_t
  |> print_s;
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
