open! Core
open! Desmos_compiler
open! Languages
open! Types

let compile_to_vm prog =
  prog |> Pass_analyze_call_liveness.compile
  |> Pass_convert_functions_to_stack.compile
  |> Pass_make_program_counter_explicit.compile
  |> Pass_prepare_registers.compile

let prog_fib input_n =
  let open Register_func_instrs in
  let fib = Function_name.of_string "fib" in
  let fib_label = Label.of_string "fib" in
  let n = Register.of_string "n" in
  let a = Register.of_string "a" in
  let b = Register.of_string "b" in
  let result = Register.of_string "result" in
  let base_label = Label.of_string "fib_base" in
  let recurse_label = Label.of_string "fib_recurse" in
  {
    functions =
      Function_name.Map.of_alist_exn
        [
          ( fib,
            {
              entry_label = fib_label;
              params = [ n; a; b ];
              blocks =
                [
                  {
                    label = fib_label;
                    body = [];
                    control_flow =
                      Jump
                        {
                          conds =
                            [ (Compare (Eq, Register n, Num 0.), base_label) ];
                          default = recurse_label;
                        };
                  };
                  {
                    label = base_label;
                    body = [];
                    control_flow = Return (Register a);
                  };
                  {
                    label = recurse_label;
                    body =
                      [
                        Call
                          {
                            func_name = fib;
                            args =
                              [
                                Sub (Register n, Num 1.);
                                Register b;
                                Add (Register a, Register b);
                              ];
                            ret = Some a;
                          };
                      ];
                    control_flow = Return (Register a);
                  };
                ];
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
                  func_name = fib;
                  args = [ Num (float_of_int input_n); Num 0.; Num 1. ];
                  ret = Some result;
                };
            ];
          control_flow = Exit;
        };
      ];
  }

let prog_gcd input_a input_b =
  let open Register_func_instrs in
  let gcd = Function_name.of_string "gcd" in
  let gcd_label = Label.of_string "gcd" in
  let a = Register.of_string "a" in
  let b = Register.of_string "b" in
  let r = Register.of_string "r" in
  let result = Register.of_string "result" in
  let base_label = Label.of_string "gcd_base" in
  let recurse_label = Label.of_string "gcd_recurse" in
  {
    functions =
      Function_name.Map.of_alist_exn
        [
          ( gcd,
            {
              entry_label = gcd_label;
              params = [ a; b ];
              blocks =
                [
                  {
                    label = gcd_label;
                    body = [];
                    control_flow =
                      Jump
                        {
                          conds =
                            [ (Compare (Eq, Register b, Num 0.), base_label) ];
                          default = recurse_label;
                        };
                  };
                  {
                    label = base_label;
                    body = [];
                    control_flow = Return (Register a);
                  };
                  {
                    label = recurse_label;
                    body =
                      [
                        Set (r, Mod (Register a, Register b));
                        Call
                          {
                            func_name = gcd;
                            args = [ Register b; Register r ];
                            ret = Some result;
                          };
                      ];
                    control_flow = Return (Register result);
                  };
                ];
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
                  func_name = gcd;
                  args =
                    [ Num (float_of_int input_a); Num (float_of_int input_b) ];
                  ret = Some result;
                };
            ];
          control_flow = Exit;
        };
      ];
  }
