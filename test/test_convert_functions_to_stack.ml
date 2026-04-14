open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "test register saving in functions" =
  let prog : Register_func_instrs.t =
    let open Register_func_instrs in
    let f = Function_name.of_string "f" in
    let x = Register.of_string "x" in
    let y = Register.of_string "y" in
    let z = Register.of_string "z" in
    let a = Register.of_string "a" in
    let b = Register.of_string "b" in
    {
      functions =
        Function_name.Map.of_alist_exn
          [
            ( f,
              {
                params = [ y; z ];
                body =
                  [
                    Set (a, Add (Register y, Register z));
                    Set (b, Mult (Register a, Register z));
                    Return (Register b);
                  ];
              } );
          ];
      main =
        [
          Set (x, Num 5.);
          Set (y, Num 3.);
          Set (z, Num 2.);
          Call
            { func_name = f; args = [ Register y; Register x ]; ret = Some a };
          Set (b, Num 99.);
        ];
    }
  in
  prog |> Pass_convert_functions_to_stack.compile
  |> Register_stack_instrs.sexp_of_t |> print_s;
  (* y/z should be PushAndSet, a/b/x should be Push in the fucntion call*)
  (* then they should all be Pop afterwards *)
  [%expect
    {|
    ((GeneralizedSet ((x (Set (Num 5))))) (GeneralizedSet ((y (Set (Num 3)))))
     (GeneralizedSet ((z (Set (Num 2)))))
     (GeneralizedSet
      ((y (PushAndSet (Register y))) (z (PushAndSet (Register x))) (a Push)
       (b Push) (x Push)))
     (Link_push_jump function_entrypoint_f)
     (GeneralizedSet ((a Pop) (b Pop) (x Pop) (y Pop) (z Pop)))
     (GeneralizedSet ((a (Set (Register .ret)))))
     (GeneralizedSet ((b (Set (Num 99))))) Exit (Label function_entrypoint_f)
     (GeneralizedSet ((a (Set (Add (Register y) (Register z))))))
     (GeneralizedSet ((b (Set (Mult (Register a) (Register z))))))
     (GeneralizedSet ((.ret (Set (Register b))))) Link_pop_jump)
    |}]

let%expect_test "test mutual recursive functions" =
  let prog : Register_func_instrs.t =
    let open Register_func_instrs in
    let f = Function_name.of_string "f" in
    let g = Function_name.of_string "g" in
    let a = Register.of_string "a" in
    let b = Register.of_string "b" in
    let c = Register.of_string "c" in
    let d = Register.of_string "d" in
    let x = Register.of_string "x" in
    {
      functions =
        Function_name.Map.of_alist_exn
          [
            ( f,
              {
                params = [ a; b ];
                body =
                  [
                    Call
                      {
                        func_name = g;
                        args = [ Register a; Register b ];
                        ret = Some c;
                      };
                    Return (Register c);
                  ];
              } );
            ( g,
              {
                params = [ a; b ];
                body =
                  [
                    Call
                      {
                        func_name = f;
                        args = [ Register a; Register b ];
                        ret = Some d;
                      };
                    Return (Register d);
                  ];
              } );
          ];
      main =
        [
          Set (c, Num 1.);
          Set (d, Num 2.);
          Call { func_name = f; args = [ Register c; Num 1. ]; ret = Some x };
        ];
    }
  in
  prog |> Pass_convert_functions_to_stack.compile
  |> Register_stack_instrs.sexp_of_t |> print_s;
  (* the call to f from main should save d even though it is not used directly in f *)
  [%expect
    {|
    ((GeneralizedSet ((c (Set (Num 1))))) (GeneralizedSet ((d (Set (Num 2)))))
     (GeneralizedSet
      ((a (Set (Register c))) (b (Set (Num 1))) (c Push) (d Push) (x Push)))
     (Link_push_jump function_entrypoint_f)
     (GeneralizedSet ((c Pop) (d Pop) (x Pop)))
     (GeneralizedSet ((x (Set (Register .ret))))) Exit
     (Label function_entrypoint_f)
     (GeneralizedSet
      ((a (PushAndSet (Register a))) (b (PushAndSet (Register b))) (c Push)))
     (Link_push_jump function_entrypoint_g)
     (GeneralizedSet ((a Pop) (b Pop) (c Pop)))
     (GeneralizedSet ((c (Set (Register .ret)))))
     (GeneralizedSet ((.ret (Set (Register c))))) Link_pop_jump
     (Label function_entrypoint_g)
     (GeneralizedSet
      ((a (PushAndSet (Register a))) (b (PushAndSet (Register b))) (d Push)))
     (Link_push_jump function_entrypoint_f)
     (GeneralizedSet ((a Pop) (b Pop) (d Pop)))
     (GeneralizedSet ((d (Set (Register .ret)))))
     (GeneralizedSet ((.ret (Set (Register d))))) Link_pop_jump)
    |}]
