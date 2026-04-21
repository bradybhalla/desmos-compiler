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
          {
            label = Label.of_string "main";
            body =
              [
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
          };
        ];
      info = ();
    }
  in
  (Pass_extract_registers.compile prog).info
  |> Register.Map.sexp_of_t Desmos_virtual_machine.sexp_of_expr
  |> print_s;
  [%expect
    {|
    ((00pc (Num 0)) (a (Num 1.2345)) (b (Num 1.2345)) (c (Num 1.2345))
     (d (Num 1.2345)) (e (Num 1.2345)) (f (Num 1.2345)))
    |}]
