open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "fib(12)" =
  Utils.read_from_file "programs/fib_12.sexp"
  |> Utils.compile_frontend_to_vm |> Utils.compile_vm_to_javascript
  |> print_endline;
  [%expect
    {| Calc.setExpressions([{latex: "\\left\\{R_{00pc} = 0: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{n}\\to 12\\right), R_{00pc} = 1: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{a}\\to 0\\right), R_{00pc} = 2: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{b}\\to 1\\right), R_{00pc} = 3: R_{00pc}\\to \\left\\{R_{n} > 0: 4, 9\\right\\}, R_{00pc} = 4: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{tmp}\\to R_{a}\\right), R_{00pc} = 5: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{a}\\to R_{b}\\right), R_{00pc} = 6: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{b}\\to \\left(R_{a} + R_{tmp}\\right)\\right), R_{00pc} = 7: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{n}\\to \\left(R_{n} - 1\\right)\\right), R_{00pc} = 8: R_{00pc}\\to \\left\\{R_{n} > 0: 4, 9\\right\\}, R_{00pc} = 9: \\left(R_{00pc}\\to \\left(R_{00pc} + 1\\right), R_{result}\\to R_{a}\\right), R_{00pc} = 10: R_{00pc}\\to -1, R_{00pc}\\to R_{00pc}\\right\\}"}, {latex: "R_{00pc}=0"}, {latex: "R_{00pcStack}=\\left[5.4321\\right]"}, {latex: "R_{a}=1.2345"}, {latex: "R_{aStack}=\\left[5.4321\\right]"}, {latex: "R_{b}=1.2345"}, {latex: "R_{bStack}=\\left[5.4321\\right]"}, {latex: "R_{n}=1.2345"}, {latex: "R_{nStack}=\\left[5.4321\\right]"}, {latex: "R_{result}=1.2345"}, {latex: "R_{resultStack}=\\left[5.4321\\right]"}, {latex: "R_{tmp}=1.2345"}, {latex: "R_{tmpStack}=\\left[5.4321\\right]"}]) |}]
