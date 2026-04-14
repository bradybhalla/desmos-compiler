open! Core
open! Desmos_compiler
open! Languages
open! Types

(* A program that calls a function f(x, y) = x + y with x=3, y=4.
   The result is stored in register `result`.

   Layout:
     0: x = 3, y = 4
     1: push x, push y, push link=(PC+1), jump to function_entrypoint_f
     2: pop y, pop x
     3: result = .ret
     4: (Label function_entrypoint_f)
     5: .ret = x + y
     6: pop .link, jump to .link
*)

let add_program : Desmos_virtual_machine.t =
  let open Desmos_virtual_machine in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  let result = Register.of_string "result" in
  let ret = Register.of_string ".ret" in
  let link = link_register in
  let f_label = Label.of_string "function_entrypoint_f" in
  {
    registers = Register.Set.of_list [ x; y; result; ret; link ];
    main =
      [
        (* x = 3, y = 4 *)
        Instruction ([ (x, Set (Num 3.)); (y, Set (Num 4.)) ], NextInstr);
        (* save x and y to stack, push link = PC+1, jump to f *)
        Instruction
          ( [
              (x, PushAndSet (Register x));
              (y, PushAndSet (Register y));
              (link, PushAndSet (Add (ProgramCounter, Num 1.)));
            ],
            Jump { conds = []; default = JumpToLabel f_label } );
        (* restore y, x from stack *)
        Instruction ([ (y, Pop); (x, Pop) ], NextInstr);
        (* result = .ret *)
        Instruction ([ (result, Set (Register ret)) ], NextInstr);
        Instruction ([], Exit);
        (* function body: f(x, y) = x + y *)
        Label f_label;
        Instruction ([ (ret, Set (Add (Register x, Register y))) ], NextInstr);
        (* pop link and jump to it (return) *)
        Instruction
          ([ (link, Pop) ], Jump { conds = []; default = JumpToRegister link });
      ];
  }

let%expect_test "print add program" =
  add_program |> Desmos_virtual_machine.sexp_of_t |> print_s;
  [%expect
    {|
    ((main
      ((Instruction (((x (Set (Num 3))) (y (Set (Num 4)))) NextInstr))
       (Instruction
        (((x (PushAndSet (Register x))) (y (PushAndSet (Register y)))
          (.link (PushAndSet (Add ProgramCounter (Num 1)))))
         (Jump (conds ()) (default (JumpToLabel function_entrypoint_f)))))
       (Instruction (((y Pop) (x Pop)) NextInstr))
       (Instruction (((result (Set (Register .ret)))) NextInstr))
       (Instruction (() Exit)) (Label function_entrypoint_f)
       (Instruction (((.ret (Set (Add (Register x) (Register y))))) NextInstr))
       (Instruction
        (((.link Pop)) (Jump (conds ()) (default (JumpToRegister .link)))))))
     (registers (.link .ret result x y)))
    |}]

let%expect_test "run add program" =
  let print_registers (t : Desmos_vm_emulator.t) =
    Hashtbl.to_alist t.registers
    |> List.iter ~f:(fun (r, v) ->
           Printf.printf "Reg: %s, Value: %.2f (" (Register.to_string r) v;
           List.iter (Hashtbl.find_exn t.register_stacks r) ~f:(fun v ->
               Printf.printf "%.2f " v);
           Printf.printf ")\n")
  in
  let rec run_until_done t =
    let res = Desmos_vm_emulator.step t in
    print_registers t;
    print_endline "";
    match res with `Done -> () | `Not_done -> run_until_done t
  in
  let t = Desmos_vm_emulator.create add_program in
  run_until_done t;
  [%expect {|
    (((x (Set (Num 3))) (y (Set (Num 4)))) NextInstr)
    Reg: result, Value: 0.00 ()
    Reg: y, Value: 4.00 ()
    Reg: .ret, Value: 0.00 ()
    Reg: x, Value: 3.00 ()
    Reg: .link, Value: 0.00 ()

    (((x (PushAndSet (Register x))) (y (PushAndSet (Register y)))
      (.link (PushAndSet (Add ProgramCounter (Num 1)))))
     (Jump (conds ()) (default (JumpToLabel function_entrypoint_f))))
    Reg: result, Value: 0.00 ()
    Reg: y, Value: 4.00 (4.00 )
    Reg: .ret, Value: 0.00 ()
    Reg: x, Value: 3.00 (3.00 )
    Reg: .link, Value: 2.00 (0.00 )

    (((.ret (Set (Add (Register x) (Register y))))) NextInstr)
    Reg: result, Value: 0.00 ()
    Reg: y, Value: 4.00 (4.00 )
    Reg: .ret, Value: 7.00 ()
    Reg: x, Value: 3.00 (3.00 )
    Reg: .link, Value: 2.00 (0.00 )

    (((.link Pop)) (Jump (conds ()) (default (JumpToRegister .link))))
    Reg: result, Value: 0.00 ()
    Reg: y, Value: 4.00 (4.00 )
    Reg: .ret, Value: 7.00 ()
    Reg: x, Value: 3.00 (3.00 )
    Reg: .link, Value: 0.00 ()

    (((y Pop) (x Pop)) NextInstr)
    Reg: result, Value: 0.00 ()
    Reg: y, Value: 4.00 ()
    Reg: .ret, Value: 7.00 ()
    Reg: x, Value: 3.00 ()
    Reg: .link, Value: 0.00 ()

    (((result (Set (Register .ret)))) NextInstr)
    Reg: result, Value: 7.00 ()
    Reg: y, Value: 4.00 ()
    Reg: .ret, Value: 7.00 ()
    Reg: x, Value: 3.00 ()
    Reg: .link, Value: 0.00 ()

    (() Exit)
    Reg: result, Value: 7.00 ()
    Reg: y, Value: 4.00 ()
    Reg: .ret, Value: 7.00 ()
    Reg: x, Value: 3.00 ()
    Reg: .link, Value: 0.00 ()
    |}]
