open! Core
open! Desmos_compiler
open! Languages
open! Types

let%expect_test "parse a simple program" =
  let ast = Utils.read_from_file "programs/basic_language_features.sexp" in
  ast |> C_style_frontend.sexp_of_t |> print_s;
  [%expect
    {|
    ((Function_def f (x y)
      ((Set z (Add (Register x) (Register y)))
       (Set z (Mult (Register z) (Num 2))) (Return (Register z))))
     (Set x (Call f ((Num 1) (Num 4))))
     (Set y (Call f ((Register x) (Register x)))) (Set z (Num 0))
     (If
      (branches
       (((Compare Gt (Register y) (Mult (Num 200) (Register x)))
         ((Set z (Num 1))))
        ((Compare Gt (Register y) (Register x)) ((Set z (Num 2))))))
      (else_ ((Set z (Num 3)))))
     (Function_def isPositive (w) ((Return (Compare Gt (Register w) (Num 0)))))
     (Set w (Num 0))
     (While (Call isPositive ((Register z)))
      ((Set w (Add (Register w) (Num 1))) (Set z (Sub (Register z) (Num 1)))))
     (Set a
      (If_expr
       (conds
        (((Compare Eq (Register w) (Num 1)) (Num 5))
         ((Compare Eq (Register w) (Num 2)) (Num 6))
         ((Compare Eq (Register w) (Num 3)) (Num 7))))
       (default (Num 8)))))
    |}]

let%expect_test "parse an invalid function" =
  let prog = {|
    (set x (set x 1))
  |} in
  let ast = Utils.read_from_str prog in
  ast |> C_style_frontend.sexp_of_t |> print_s;
  [%expect {||}]
