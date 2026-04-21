open! Core
open! Desmos_compiler
open! Languages
open! Types

(*

let compile prog =
  prog |> Pass_analyze_call_liveness.compile
  |> Pass_convert_functions_to_stack.compile
  |> Pass_explicit_program_counter.compile |> Pass_extract_registers.compile

let print_register t register_name =
  let r = Register.of_string register_name in
  let v = Hashtbl.find_exn t.Desmos_vm_emulator.registers r in
  Printf.printf "%.1f\n" v

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
          label = entry_label;
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

let%expect_test "fib" =
  let t = prog_fib 0 |> compile |> Desmos_vm_emulator.run_until_done in
  print_register t "result";
  [%expect {| 0.0 |}];
  let t = prog_fib 2 |> compile |> Desmos_vm_emulator.run_until_done in
  print_register t "result";
  [%expect {| 1.0 |}];
  let t = prog_fib 12 |> compile |> Desmos_vm_emulator.run_until_done in
  print_register t "result";
  [%expect {| 144.0 |}]

let%expect_test "gcd" =
  let t = prog_gcd 6 9 |> compile |> Desmos_vm_emulator.run_until_done in
  print_register t "result";
  [%expect {| 3.0 |}];
  let t = prog_gcd 432 123 |> compile |> Desmos_vm_emulator.run_until_done in
  print_register t "result";
  [%expect {| 3.0 |}];
  let t = prog_gcd 432 1231 |> compile |> Desmos_vm_emulator.run_until_done in
  print_register t "result";
  [%expect {| 1.0 |}]
  *)
