(* TODO: remove this file, I'm just using this to run the compiler because its easier than making an executable for now *)

open! Core
open! Desmos_compiler
open! Languages
open! Types

let compile prog =
  prog |> Pass_convert_functions_to_stack.compile
  |> Pass_explicit_program_counter.compile
  |> Pass_generate_desmos_output.compile
  |> Languages.Desmos_output.to_pastable_javascript

let prog_fib input_n =
  let open Register_func_instrs in
  let fib = Function_name.of_string "fib" in
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
              params = [ n; a; b ];
              blocks =
                [
                  {
                    label = entry_label;
                    body =
                      [
                        Jump
                          {
                            conds =
                              [ (Compare (Eq, Register n, Num 0.), base_label) ];
                            default = recurse_label;
                          };
                      ];
                  };
                  { label = base_label; body = [ Return (Register a) ] };
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
                        Return (Register result);
                      ];
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
                  func_name = fib;
                  args = [ Num (float_of_int input_n); Num 0.; Num 1. ];
                  ret = Some result;
                };
            ];
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
              blocks =
                [
                  {
                    label = entry_label;
                    body =
                      [
                        Jump
                          {
                            conds =
                              [
                                (Compare (Eq, Register b, Num 0.), base_label);
                              ];
                            default = recurse_label;
                          };
                      ];
                  };
                  { label = base_label; body = [ Return (Register a) ] };
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
                        Return (Register result);
                      ];
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
                    [
                      Num (float_of_int input_a); Num (float_of_int input_b);
                    ];
                  ret = Some result;
                };
            ];
        };
      ];
  }

let%expect_test "fib" =
  prog_fib 12 |> compile |> print_endline;
  [%expect
    {| Calc.setExpressions([{latex: "\\left\\{R_{00pc} = 0: \\left(R_{n}\\to 12, R_{a}\\to 0, R_{b}\\to 1, R_{resultStack}\\to \\operatorname{join}\\left(R_{resultStack}, R_{result}\\right), R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 1: \\left(R_{00linkStack}\\to \\operatorname{join}\\left(R_{00linkStack}, R_{00link}\\right), R_{00link}\\to \\left(R_{00pc} + 1\\right), R_{00pc}\\to 5\\right), R_{00pc} = 2: \\left(R_{result}\\to R_{resultStack}\\left[\\operatorname{length}\\left(R_{resultStack}\\right)\\right], R_{resultStack}\\to R_{resultStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{resultStack}\\right) - 1\\right)\\right], R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 3: \\left(R_{result}\\to R_{00ret}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 4: R_{00pc}\\to -1, R_{00pc} = 5: R_{00pc}\\to \\left\\{R_{n} = 0: 6, 8\\right\\}, R_{00pc} = 6: \\left(R_{00ret}\\to R_{a}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 7: \\left(R_{00link}\\to R_{00linkStack}\\left[\\operatorname{length}\\left(R_{00linkStack}\\right)\\right], R_{00linkStack}\\to R_{00linkStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{00linkStack}\\right) - 1\\right)\\right], R_{00pc}\\to R_{00link}\\right), R_{00pc} = 8: \\left(R_{nStack}\\to \\operatorname{join}\\left(R_{nStack}, R_{n}\\right), R_{n}\\to \\left(R_{n} - 1\\right), R_{aStack}\\to \\operatorname{join}\\left(R_{aStack}, R_{a}\\right), R_{a}\\to R_{b}, R_{bStack}\\to \\operatorname{join}\\left(R_{bStack}, R_{b}\\right), R_{b}\\to \\left(R_{a} + R_{b}\\right), R_{resultStack}\\to \\operatorname{join}\\left(R_{resultStack}, R_{result}\\right), R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 9: \\left(R_{00linkStack}\\to \\operatorname{join}\\left(R_{00linkStack}, R_{00link}\\right), R_{00link}\\to \\left(R_{00pc} + 1\\right), R_{00pc}\\to 5\\right), R_{00pc} = 10: \\left(R_{a}\\to R_{aStack}\\left[\\operatorname{length}\\left(R_{aStack}\\right)\\right], R_{aStack}\\to R_{aStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{aStack}\\right) - 1\\right)\\right], R_{b}\\to R_{bStack}\\left[\\operatorname{length}\\left(R_{bStack}\\right)\\right], R_{bStack}\\to R_{bStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{bStack}\\right) - 1\\right)\\right], R_{n}\\to R_{nStack}\\left[\\operatorname{length}\\left(R_{nStack}\\right)\\right], R_{nStack}\\to R_{nStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{nStack}\\right) - 1\\right)\\right], R_{result}\\to R_{resultStack}\\left[\\operatorname{length}\\left(R_{resultStack}\\right)\\right], R_{resultStack}\\to R_{resultStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{resultStack}\\right) - 1\\right)\\right], R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 11: \\left(R_{result}\\to R_{00ret}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 12: \\left(R_{00ret}\\to R_{result}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 13: \\left(R_{00link}\\to R_{00linkStack}\\left[\\operatorname{length}\\left(R_{00linkStack}\\right)\\right], R_{00linkStack}\\to R_{00linkStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{00linkStack}\\right) - 1\\right)\\right], R_{00pc}\\to R_{00link}\\right), R_{00pc}\\to -1\\right\\}"}, {latex: "R_{00pc}=0"}, {latex: "R_{00link}=5.4321"}, {latex: "R_{00linkStack}=\\left[1.2345\\right]"}, {latex: "R_{00ret}=5.4321"}, {latex: "R_{00retStack}=\\left[1.2345\\right]"}, {latex: "R_{a}=5.4321"}, {latex: "R_{aStack}=\\left[1.2345\\right]"}, {latex: "R_{b}=5.4321"}, {latex: "R_{bStack}=\\left[1.2345\\right]"}, {latex: "R_{n}=5.4321"}, {latex: "R_{nStack}=\\left[1.2345\\right]"}, {latex: "R_{result}=5.4321"}, {latex: "R_{resultStack}=\\left[1.2345\\right]"}]) |}]

let%expect_test "gcd" =
  prog_gcd 432 1231 |> compile |> print_endline;
  [%expect
    {| Calc.setExpressions([{latex: "\\left\\{R_{00pc} = 0: \\left(R_{a}\\to 432, R_{b}\\to 1231, R_{resultStack}\\to \\operatorname{join}\\left(R_{resultStack}, R_{result}\\right), R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 1: \\left(R_{00linkStack}\\to \\operatorname{join}\\left(R_{00linkStack}, R_{00link}\\right), R_{00link}\\to \\left(R_{00pc} + 1\\right), R_{00pc}\\to 5\\right), R_{00pc} = 2: \\left(R_{result}\\to R_{resultStack}\\left[\\operatorname{length}\\left(R_{resultStack}\\right)\\right], R_{resultStack}\\to R_{resultStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{resultStack}\\right) - 1\\right)\\right], R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 3: \\left(R_{result}\\to R_{00ret}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 4: R_{00pc}\\to -1, R_{00pc} = 5: R_{00pc}\\to \\left\\{R_{b} = 0: 6, 8\\right\\}, R_{00pc} = 6: \\left(R_{00ret}\\to R_{a}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 7: \\left(R_{00link}\\to R_{00linkStack}\\left[\\operatorname{length}\\left(R_{00linkStack}\\right)\\right], R_{00linkStack}\\to R_{00linkStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{00linkStack}\\right) - 1\\right)\\right], R_{00pc}\\to R_{00link}\\right), R_{00pc} = 8: \\left(R_{r}\\to \\operatorname{mod}\\left(R_{a}, R_{b}\\right), R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 9: \\left(R_{aStack}\\to \\operatorname{join}\\left(R_{aStack}, R_{a}\\right), R_{a}\\to R_{b}, R_{bStack}\\to \\operatorname{join}\\left(R_{bStack}, R_{b}\\right), R_{b}\\to R_{r}, R_{rStack}\\to \\operatorname{join}\\left(R_{rStack}, R_{r}\\right), R_{resultStack}\\to \\operatorname{join}\\left(R_{resultStack}, R_{result}\\right), R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 10: \\left(R_{00linkStack}\\to \\operatorname{join}\\left(R_{00linkStack}, R_{00link}\\right), R_{00link}\\to \\left(R_{00pc} + 1\\right), R_{00pc}\\to 5\\right), R_{00pc} = 11: \\left(R_{a}\\to R_{aStack}\\left[\\operatorname{length}\\left(R_{aStack}\\right)\\right], R_{aStack}\\to R_{aStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{aStack}\\right) - 1\\right)\\right], R_{b}\\to R_{bStack}\\left[\\operatorname{length}\\left(R_{bStack}\\right)\\right], R_{bStack}\\to R_{bStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{bStack}\\right) - 1\\right)\\right], R_{r}\\to R_{rStack}\\left[\\operatorname{length}\\left(R_{rStack}\\right)\\right], R_{rStack}\\to R_{rStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{rStack}\\right) - 1\\right)\\right], R_{result}\\to R_{resultStack}\\left[\\operatorname{length}\\left(R_{resultStack}\\right)\\right], R_{resultStack}\\to R_{resultStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{resultStack}\\right) - 1\\right)\\right], R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 12: \\left(R_{result}\\to R_{00ret}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 13: \\left(R_{00ret}\\to R_{result}, R_{00pc}\\to \\left(R_{00pc} + 1\\right)\\right), R_{00pc} = 14: \\left(R_{00link}\\to R_{00linkStack}\\left[\\operatorname{length}\\left(R_{00linkStack}\\right)\\right], R_{00linkStack}\\to R_{00linkStack}\\left[1 ... \\left(\\operatorname{length}\\left(R_{00linkStack}\\right) - 1\\right)\\right], R_{00pc}\\to R_{00link}\\right), R_{00pc}\\to -1\\right\\}"}, {latex: "R_{00pc}=0"}, {latex: "R_{00link}=5.4321"}, {latex: "R_{00linkStack}=\\left[1.2345\\right]"}, {latex: "R_{00ret}=5.4321"}, {latex: "R_{00retStack}=\\left[1.2345\\right]"}, {latex: "R_{a}=5.4321"}, {latex: "R_{aStack}=\\left[1.2345\\right]"}, {latex: "R_{b}=5.4321"}, {latex: "R_{bStack}=\\left[1.2345\\right]"}, {latex: "R_{r}=5.4321"}, {latex: "R_{rStack}=\\left[1.2345\\right]"}, {latex: "R_{result}=5.4321"}, {latex: "R_{resultStack}=\\left[1.2345\\right]"}]) |}]
