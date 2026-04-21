open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "registers extracted from stmts, exprs, and jumps" =
  let prog : unit Desmos_virtual_machine.t =
    let open Desmos_virtual_machine in
    {
      main =
        [
          Label (Label.of_string "main");
          Instruction
            [
              ( Register.of_string "b",
                Set (Add (Register (Register.of_string "a"), Num 1.)) );
            ];
          Instruction
            [
              ( program_counter_reg,
                Set
                  (If_expr
                     {
                       conds =
                         [
                           ( Compare
                               ( Compare_op.Eq,
                                 Register (Register.of_string "c"),
                                 Num 0. ),
                             Register (Register.of_string "d") );
                         ];
                       default = Register (Register.of_string "e");
                     }) );
            ];
          Instruction [ (Register.of_string "f", Pop) ];
          Exit;
        ];
      info = ();
    }
  in
  prog |> Pass_extract_registers.compile
  |> Desmos_virtual_machine.sexp_of_t Register.Set.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((main
      ((Label main) (Instruction ((b (Set (Add (Register a) (Num 1))))))
       (Instruction
        ((00pc
          (Set
           (If_expr (conds (((Compare Eq (Register c) (Num 0)) (Register d))))
            (default (Register e)))))))
       (Instruction ((f Pop))) Exit))
     (info (00pc a b c d e f)))
    |}]
