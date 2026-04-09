open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "compile arithmetic program to desmos vm" =
  Test_convert_functions_to_stack.prog1
  |> Pass_convert_functions_to_stack.compile
  |> Pass_explicit_program_counter.compile |> Desmos_virtual_machine.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((main
      ((Instruction (((x (Set (Num 1)))) NextInstr))
       (Instruction (((y (Set (Num 2)))) NextInstr))
       (Instruction (((z (Set (Num 2)))) NextInstr))
       (Instruction
        (((x (PushAndSet (Register x))) (y (PushAndSet (Register y)))
          (z (PushAndSet (Register y))))
         NextInstr))
       (Instruction
        (((.link (PushAndSet (Add ProgramCounter (Num 1)))))
         (Jump (conds ()) (default (JumpToLabel function_entrypoint_g)))))
       (Instruction (((x Pop) (y Pop) (z Pop)) NextInstr))
       (Instruction (((x (Set (Register .ret)))) NextInstr))
       (Label function_entrypoint_f)
       (Instruction (((z (Set (Add (Register z) (Register x))))) NextInstr))
       (Instruction (((z (Set (Add (Register z) (Register y))))) NextInstr))
       (Instruction (((.ret (Set (Register z)))) NextInstr))
       (Instruction
        (((.link Pop)) (Jump (conds ()) (default (JumpToRegister .link)))))
       (Label function_entrypoint_g)
       (Instruction
        (((x (PushAndSet (Register x))) (y (PushAndSet (Register y)))
          (z (PushAndSet (Register y))) (w Push))
         NextInstr))
       (Instruction
        (((.link (PushAndSet (Add ProgramCounter (Num 1)))))
         (Jump (conds ()) (default (JumpToLabel function_entrypoint_f)))))
       (Instruction (((w Pop) (x Pop) (y Pop) (z Pop)) NextInstr))
       (Instruction (((w (Set (Register .ret)))) NextInstr))
       (Instruction
        (((.ret (Set (Mult (Register x) (Mult (Register y) (Register z))))))
         NextInstr))
       (Instruction
        (((.link Pop)) (Jump (conds ()) (default (JumpToRegister .link)))))))
     (registers (.link .ret w x y z)))
    |}]
