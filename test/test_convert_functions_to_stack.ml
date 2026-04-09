open! Core
open! Desmos_compiler
open! Languages
open! Types

(* TODO brady: these tests don't check anything interesting yet *)

let prog1 : Register_func_instrs.t =
  let open Register_func_instrs in
  let f = Function_name.of_string "f" in
  let g = Function_name.of_string "g" in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  let z = Register.of_string "z" in
  let w = Register.of_string "w" in
  {
    functions =
      Function_name.Map.of_alist_exn
        [
          ( f,
            {
              params = [ x; y; z ];
              body =
                [
                  Set (z, Add (Register z, Register x));
                  Set (z, Add (Register z, Register y));
                  Return (Register z);
                ];
            } );
          ( g,
            {
              params = [ x; y; z ];
              body =
                [
                  Call
                    {
                      func_name = f;
                      args = [ Register x; Register y; Register y ];
                      ret = Some w;
                    };
                  Return (Mult (Register x, Mult (Register y, Register z)));
                ];
            } );
        ];
    main =
      [
        Set (x, Num 1.);
        Set (y, Num 2.);
        Set (z, Num 2.);
        Call
          {
            func_name = g;
            args = [ Register x; Register y; Register y ];
            ret = Some x;
          };
      ];
  }

let%expect_test "print program" =
  prog1 |> Register_func_instrs.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions
      ((f
        ((params (x y z))
         (body
          ((Set z (Add (Register z) (Register x)))
           (Set z (Add (Register z) (Register y))) (Return (Register z))))))
       (g
        ((params (x y z))
         (body
          ((Call (func_name f) (args ((Register x) (Register y) (Register y)))
            (ret (w)))
           (Return (Mult (Register x) (Mult (Register y) (Register z))))))))))
     (main
      ((Set x (Num 1)) (Set y (Num 2)) (Set z (Num 2))
       (Call (func_name g) (args ((Register x) (Register y) (Register y)))
        (ret (x))))))
    |}]

let%expect_test "compile arithmetic program" =
  prog1 |> Pass_convert_functions_to_stack.compile
  |> Register_stack_instrs.sexp_of_t |> print_s;
  [%expect
    {|
    ((Set x (Num 1)) (Set y (Num 2)) (Set z (Num 2))
     (GeneralizedSet
      ((x (PushAndSet (Register x))) (y (PushAndSet (Register y)))
       (z (PushAndSet (Register y)))))
     (Link_push_jump function_entrypoint_g)
     (GeneralizedSet ((x Pop) (y Pop) (z Pop))) (Set x (Register .ret))
     (Label function_entrypoint_f) (Set z (Add (Register z) (Register x)))
     (Set z (Add (Register z) (Register y))) (Set .ret (Register z))
     Link_pop_jump (Label function_entrypoint_g)
     (GeneralizedSet
      ((x (PushAndSet (Register x))) (y (PushAndSet (Register y)))
       (z (PushAndSet (Register y))) (w Push)))
     (Link_push_jump function_entrypoint_f)
     (GeneralizedSet ((w Pop) (x Pop) (y Pop) (z Pop))) (Set w (Register .ret))
     (Set .ret (Mult (Register x) (Mult (Register y) (Register z))))
     Link_pop_jump)
    |}]
