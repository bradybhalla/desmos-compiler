open! Core
open! Desmos_compiler
open! Languages.Register_func_instrs
open! Types

let prog1 : t =
  let f = Function_name.of_string "f" in
  let g = Function_name.of_string "g" in
  let x = Register.of_string "x" in
  let y = Register.of_string "y" in
  let z = Register.of_string "z" in
  let w = Register.of_string "w" in
  {
    functions =
      [
        {
          name = f;
          params = [ x; y; z ];
          body =
            [
              Set (z, Add (Register z, Register x));
              Set (z, Add (Register z, Register y));
              Return (Register z);
            ];
        };
        {
          name = g;
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
        };
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
  prog1 |> sexp_of_t |> print_s;
  [%expect
    {|
    ((functions
      (((name f) (params (x y z))
        (body
         ((Set z (Add (Register z) (Register x)))
          (Set z (Add (Register z) (Register y))) (Return (Register z)))))
       ((name g) (params (x y z))
        (body
         ((Call (func_name f) (args ((Register x) (Register y) (Register y)))
           (ret (w)))
          (Return (Mult (Register x) (Mult (Register y) (Register z)))))))))
     (main ()))
    |}]
