open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "registers before and after call show up as live" =
  let prog : Register_func_instrs.t =
    let open Register_func_instrs in
    let f = Function_name.of_string "f" in
    let f_label = Label.of_string "f" in
    let y = Register.of_string "y" in
    let z = Register.of_string "z" in
    let p = Register.of_string "p" in
    {
      functions =
        Function_name.Map.of_alist_exn
          [
            ( f,
              {
                entry_label = f_label;
                params = [ p ];
                blocks =
                  [
                    {
                      label = f_label;
                      body = [];
                      control_flow = Return (Register p);
                    };
                  ];
              } );
          ];
      main =
        [
          {
            label = Label.of_string "main";
            body =
              [
                Set (Register.of_string "x", Num 5.);
                Set (y, Num 3.);
                Call { func_name = f; args = [ Register y ]; ret = Some z };
                Set (Register.of_string "w", Register z);
              ];
            control_flow = Exit;
          };
        ];
    }
  in
  prog |> Pass_analyze_call_liveness.compile
  |> Register_func_instrs_with_call_liveness.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions
      ((f
        ((entry_label f) (params (p))
         (blocks (((label f) (body ()) (control_flow (Return (Register p))))))))))
     (main
      (((label main)
        (body
         ((Set x (Num 5)) (Set y (Num 3))
          (Call (func_name f) (args ((Register y))) (ret (z))
           (live_registers (w x y z)))
          (Set w (Register z))))
        (control_flow Exit)))))
    |}]

let%expect_test "registers in a function show up with nested calls" =
  let prog : Register_func_instrs.t =
    let open Register_func_instrs in
    let f = Function_name.of_string "f" in
    let g = Function_name.of_string "g" in
    let f_label = Label.of_string "f" in
    let g_label = Label.of_string "g" in
    let a = Register.of_string "a" in
    let b = Register.of_string "b" in
    let r = Register.of_string "r" in
    {
      functions =
        Function_name.Map.of_alist_exn
          [
            ( f,
              {
                entry_label = f_label;
                params = [ a ];
                blocks =
                  [
                    {
                      label = f_label;
                      body =
                        [
                          Set (b, Add (Register a, Num 1.));
                          Call
                            {
                              func_name = g;
                              args = [ Register b ];
                              ret = Some r;
                            };
                        ];
                      control_flow = Return (Register r);
                    };
                  ];
              } );
            ( g,
              {
                entry_label = g_label;
                params = [ r ];
                blocks =
                  [
                    {
                      label = g_label;
                      body = [];
                      control_flow = Return (Register r);
                    };
                  ];
              } );
          ];
      main =
        [
          {
            label = Label.of_string "main";
            body = [ Call { func_name = f; args = [ Num 10. ]; ret = None } ];
            control_flow = Exit;
          };
        ];
    }
  in
  prog |> Pass_analyze_call_liveness.compile
  |> Register_func_instrs_with_call_liveness.sexp_of_t |> print_s;
  [%expect
    {|
    ((functions
      ((f
        ((entry_label f) (params (a))
         (blocks
          (((label f)
            (body
             ((Set b (Add (Register a) (Num 1)))
              (Call (func_name g) (args ((Register b))) (ret (r))
               (live_registers (a b r)))))
            (control_flow (Return (Register r))))))))
       (g
        ((entry_label g) (params (r))
         (blocks (((label g) (body ()) (control_flow (Return (Register r))))))))))
     (main
      (((label main)
        (body
         ((Call (func_name f) (args ((Num 10))) (ret ()) (live_registers ()))))
        (control_flow Exit)))))
    |}]
