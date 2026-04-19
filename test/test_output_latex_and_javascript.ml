open! Core
open! Desmos_compiler
open! Languages
open! Types
open Desmos_output

let stack_register = Register.of_string "stack"
let pc_register = Register.of_string "pc"

let%expect_test "check expression to latex" =
  ListSlice
    (Register stack_register, Num 1., ListLength (Register stack_register))
  |> latex_of_expr |> print_endline;
  [%expect
    {| R_{stack}\left[1 ... \operatorname{length}\left(R_{stack}\right)\right] |}]

let%expect_test "check action to latex" =
  { conds = []; default = [ (pc_register, Register pc_register) ] }
  |> latex_of_action |> print_endline;
  [%expect {| R_{pc}\to R_{pc} |}];
  {
    conds = [];
    default =
      [
        (pc_register, Register pc_register);
        (stack_register, ListJoin (Register stack_register, Num 2.));
      ];
  }
  |> latex_of_action |> print_endline;
  [%expect
    {| \left(R_{pc}\to R_{pc}, R_{stack}\to \operatorname{join}\left(R_{stack}, 2\right)\right) |}];
  {
    conds =
      [
        ( Compare (Eq, Register pc_register, Num 1.),
          [ (pc_register, Add (Register pc_register, Num 1.)) ] );
      ];
    default =
      [
        (pc_register, Register pc_register);
        (stack_register, ListJoin (Register stack_register, Num 2.));
      ];
  }
  |> latex_of_action |> print_endline;
  [%expect
    {| \left\{R_{pc} = 1: R_{pc}\to \left(R_{pc} + 1\right), \left(R_{pc}\to R_{pc}, R_{stack}\to \operatorname{join}\left(R_{stack}, 2\right)\right)\right\} |}]
