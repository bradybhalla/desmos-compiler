open! Core
open! Desmos_compiler
open! Languages
open! Types

let compile prog =
  prog |> Pass_convert_functions_to_stack.compile
  |> Pass_explicit_program_counter.compile

let print_register t register_name =
  let r = Register.of_string register_name in
  let v = Hashtbl.find_exn t.Desmos_vm_emulator.registers r in
  Printf.printf "%.1f\n" v

let prog_fib input_n =
  let open Register_func_instrs in
  let fib_tail = Function_name.of_string "fib_tail" in
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
          ( fib_tail,
            {
              params = [ n; a; b ];
              body =
                [
                  Jump
                    {
                      conds = [ (Compare (Eq, Register n, Num 0.), base_label) ];
                      default = recurse_label;
                    };
                  Label base_label;
                  Return (Register a);
                  Label recurse_label;
                  Call
                    {
                      func_name = fib_tail;
                      args =
                        [
                          Sub (Register n, Num 1.);
                          Register b;
                          Add (Register a, Register b);
                        ];
                      ret = Some result;
                    };
                  Return (Register result);
                ];
            } );
        ];
    main =
      [
        Call
          {
            func_name = fib_tail;
            args = [ Num (float_of_int input_n); Num 0.; Num 1. ];
            ret = Some result;
          };
      ];
  }

let prog_gcd input_a input_b =
  let open Register_func_instrs in
  let gcd = Function_name.of_string "gcd" in
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
              params = [ a; b ];
              body =
                [
                  Jump
                    {
                      conds = [ (Compare (Eq, Register b, Num 0.), base_label) ];
                      default = recurse_label;
                    };
                  Label base_label;
                  Return (Register a);
                  Label recurse_label;
                  Set (r, Mod (Register a, Register b));
                  Call
                    {
                      func_name = gcd;
                      args = [ Register b; Register r ];
                      ret = Some result;
                    };
                  Return (Register result);
                ];
            } );
        ];
    main =
      [
        Call
          {
            func_name = gcd;
            args = [ Num (float_of_int input_a); Num (float_of_int input_b) ];
            ret = Some result;
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
