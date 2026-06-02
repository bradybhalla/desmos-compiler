open! Core
open Desmos_compiler
open Languages

let compile prog =
  prog |> Utils.read_from_str |> Cumulative_passes.generate_javascript |> ok_exn
  |> [%sexp_of: Javascript_setup.t] |> print_s

let%expect_test "simple set values" =
  let prog =
    {|
    (decl a)
    (decl b)
    (set a 1)
    (set b (a + 2))
  |}
  in
  compile prog;
  [%expect {| "Calc.setExpressions([{latex: \"M_{ain} = \\\\left\\\\{R_{00pc} = 0: \\\\left(R_{00pc}\\\\to \\\\left(R_{00pc} + 1\\\\right), R_{a}\\\\to 1\\\\right), R_{00pc} = 1: \\\\left(R_{00pc}\\\\to \\\\left(R_{00pc} + 1\\\\right), R_{b}\\\\to \\\\left(R_{a} + 2\\\\right)\\\\right), R_{00pc} = 2: R_{00pc}\\\\to -1, R_{00pc}\\\\to R_{00pc}\\\\right\\\\}\"}, {latex: \"R_{eset} = \\\\left(R_{00pc}\\\\to 0, R_{200pcStack}\\\\to \\\\left[5.4321\\\\right], R_{00ret}\\\\to 1.2345, R_{200retStack}\\\\to \\\\left[5.4321\\\\right], R_{a}\\\\to 1.2345, R_{2aStack}\\\\to \\\\left[5.4321\\\\right], R_{b}\\\\to 1.2345, R_{2bStack}\\\\to \\\\left[5.4321\\\\right]\\\\right)\"}, {latex: \"R_{00pc}=0\"}, {latex: \"R_{200pcStack}=\\\\left[5.4321\\\\right]\"}, {latex: \"R_{00ret}=1.2345\"}, {latex: \"R_{200retStack}=\\\\left[5.4321\\\\right]\"}, {latex: \"R_{a}=1.2345\"}, {latex: \"R_{2aStack}=\\\\left[5.4321\\\\right]\"}, {latex: \"R_{b}=1.2345\"}, {latex: \"R_{2bStack}=\\\\left[5.4321\\\\right]\"}])" |}]
