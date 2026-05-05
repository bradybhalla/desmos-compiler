open! Core
open! Desmos_compiler
open! Languages
open! Types
open Desmos_output

let%expect_test "sanitize register names" =
  let r name = Register (Register.of_string name) in
  let input : [ `Unsanitized ] t =
    {
      program_action =
        {
          conds = [];
          default =
            [
              ( Register.of_string "a",
                Add
                  ( r "a",
                    Add
                      ( r "a_a",
                        Add
                          ( r "a__a",
                            Add
                              ( r "aa",
                                Add
                                  ( r "aaa",
                                    Add (r "a_a_", Add (r "_aa", r "aa_")) ) )
                          ) ) ) );
            ];
        };
      init_registers =
        List.map
          [ "a"; "a_a"; "a__a"; "aa"; "aaa"; "a_a_"; "_aa"; "aa_" ]
          ~f:(fun name -> (Register.of_string name, Num 0.));
      info = `Unsanitized;
    }
  in
  Pass_sanitize_register_names.compile input
  |> [%sexp_of: [ `Sanitized ] t]
  |> print_s;
  [%expect {|
    ((program_action
      ((conds ())
       (default
        ((a
          (Add (Register a)
           (Add (Register aa0)
            (Add (Register aa00)
             (Add (Register aa)
              (Add (Register aaa)
               (Add (Register aa000) (Add (Register aa0000) (Register aa00000)))))))))))))
     (init_registers
      ((a (Num 0)) (aa0 (Num 0)) (aa00 (Num 0)) (aa (Num 0)) (aaa (Num 0))
       (aa000 (Num 0)) (aa0000 (Num 0)) (aa00000 (Num 0))))
     (info Sanitized))
    |}]
