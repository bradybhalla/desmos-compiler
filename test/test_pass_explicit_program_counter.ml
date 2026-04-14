open! Core
open! Desmos_compiler
open! Languages
open! Types

(* This pass is pretty straightforward *)

let%expect_test "check that registers get extracted correctly" =
  let prog =
    let open Register_stack_instrs in
    let a = Register.of_string "a" in
    let b = Register.of_string "b" in
    let c = Register.of_string "c" in
    let d = Register.of_string "d" in
    let x = Register.of_string "x" in
    let ret = Register.of_string ".ret" in
    let f_label = Label.of_string "function_entrypoint_f" in
    let g_label = Label.of_string "function_entrypoint_g" in
    [
      GeneralizedSet [ (c, Set (Num 1.)) ];
      GeneralizedSet [ (d, Set (Num 2.)) ];
      GeneralizedSet
        [
          (a, Set (Register c));
          (b, Set (Num 1.));
          (c, Push);
          (d, Push);
          (x, Push);
        ];
      Link_push_jump f_label;
      GeneralizedSet [ (c, Pop); (d, Pop); (x, Pop) ];
      GeneralizedSet [ (x, Set (Register ret)) ];
      Exit;
      Label f_label;
      GeneralizedSet
        [
          (a, PushAndSet (Register a)); (b, PushAndSet (Register b)); (c, Push);
        ];
      Link_push_jump g_label;
      GeneralizedSet [ (a, Pop); (b, Pop); (c, Pop) ];
      GeneralizedSet [ (c, Set (Register ret)) ];
      GeneralizedSet [ (ret, Set (Register c)) ];
      Link_pop_jump;
      Label g_label;
      GeneralizedSet
        [
          (a, PushAndSet (Register a)); (b, PushAndSet (Register b)); (d, Push);
        ];
      Link_push_jump f_label;
      GeneralizedSet [ (a, Pop); (b, Pop); (d, Pop) ];
      GeneralizedSet [ (d, Set (Register ret)) ];
      GeneralizedSet [ (ret, Set (Register d)) ];
      Link_pop_jump;
    ]
  in
  prog |> Pass_explicit_program_counter.compile
  |> Desmos_virtual_machine.sexp_of_t |> print_s;
  [%expect {|
    ((main
      ((Instruction (((c (Set (Num 1)))) NextInstr))
       (Instruction (((d (Set (Num 2)))) NextInstr))
       (Instruction
        (((a (Set (Register c))) (b (Set (Num 1))) (c Push) (d Push) (x Push))
         NextInstr))
       (Instruction
        (((.link (PushAndSet (Add ProgramCounter (Num 1)))))
         (Jump (conds ()) (default (JumpToLabel function_entrypoint_f)))))
       (Instruction (((c Pop) (d Pop) (x Pop)) NextInstr))
       (Instruction (((x (Set (Register .ret)))) NextInstr))
       (Instruction (() Exit)) (Label function_entrypoint_f)
       (Instruction
        (((a (PushAndSet (Register a))) (b (PushAndSet (Register b))) (c Push))
         NextInstr))
       (Instruction
        (((.link (PushAndSet (Add ProgramCounter (Num 1)))))
         (Jump (conds ()) (default (JumpToLabel function_entrypoint_g)))))
       (Instruction (((a Pop) (b Pop) (c Pop)) NextInstr))
       (Instruction (((c (Set (Register .ret)))) NextInstr))
       (Instruction (((.ret (Set (Register c)))) NextInstr))
       (Instruction
        (((.link Pop)) (Jump (conds ()) (default (JumpToRegister .link)))))
       (Label function_entrypoint_g)
       (Instruction
        (((a (PushAndSet (Register a))) (b (PushAndSet (Register b))) (d Push))
         NextInstr))
       (Instruction
        (((.link (PushAndSet (Add ProgramCounter (Num 1)))))
         (Jump (conds ()) (default (JumpToLabel function_entrypoint_f)))))
       (Instruction (((a Pop) (b Pop) (d Pop)) NextInstr))
       (Instruction (((d (Set (Register .ret)))) NextInstr))
       (Instruction (((.ret (Set (Register d)))) NextInstr))
       (Instruction
        (((.link Pop)) (Jump (conds ()) (default (JumpToRegister .link)))))))
     (registers (.link .ret a b c d x)))
    |}]
