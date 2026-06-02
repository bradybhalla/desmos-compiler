open! Core
open Desmos_compiler
open Languages
open Desmos_output

let compile prog =
  prog |> Utils.read_from_str |> Cumulative_passes.sanitize_register_names
  |> ok_exn |> [%sexp_of: [ `Sanitized ] t] |> print_s

let%expect_test "sanitize register names" =
  let prog =
    {|
  (decl a)
  (decl a_a)
  (decl a__a)
  (decl aa)
  (decl aaa)
  (decl a_a_)
  (decl _aa)
  (decl aa_)
  (set a (a + (a_a + (a__a + (aa + (aaa + (a_a_ + (_aa + aa_))))))))
  |}
  in
  compile prog;
  [%expect
    {|
    ((program_action
      ((conds
        (((Eq (Register 00pc) (Num 0))
          ((00pc (Add (Register 00pc) (Num 1)))
           (a
            (Add (Register a)
             (Add (Register aa000)
              (Add (Register aa00)
               (Add (Register aa)
                (Add (Register aaa)
                 (Add (Register aa0000) (Add (Register aa0) (Register aa00000)))))))))))
         ((Eq (Register 00pc) (Num 1)) ((00pc (Num -1))))))
       (default ((00pc (Register 00pc))))))
     (init_registers
      ((00pc (Num 0)) (200pcStack (ListLiteral ((Num 5.4321))))
       (00ret (Num 1.2345)) (200retStack (ListLiteral ((Num 5.4321))))
       (aa0 (Num 1.2345)) (2aaStack0 (ListLiteral ((Num 5.4321))))
       (a (Num 1.2345)) (2aStack (ListLiteral ((Num 5.4321))))
       (aa00 (Num 1.2345)) (2aaStack00 (ListLiteral ((Num 5.4321))))
       (aa000 (Num 1.2345)) (2aaStack000 (ListLiteral ((Num 5.4321))))
       (aa0000 (Num 1.2345)) (2aaStack0000 (ListLiteral ((Num 5.4321))))
       (aa (Num 1.2345)) (2aaStack (ListLiteral ((Num 5.4321))))
       (aa00000 (Num 1.2345)) (2aaStack00000 (ListLiteral ((Num 5.4321))))
       (aaa (Num 1.2345)) (2aaaStack (ListLiteral ((Num 5.4321))))))
     (info Sanitized))
    |}]
