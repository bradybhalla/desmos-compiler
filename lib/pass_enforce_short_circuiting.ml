open! Core
open! Languages
open C_style_frontend

let rec compile_expr = function
  | Unit -> ([], Unit)
  | Register reg -> ([], Register reg)
  | Num n -> ([], Num n)
  | Bool b -> ([], Bool b)
  | Add (e1, e2) ->
      let stmts1, e1 = compile_expr e1 in
      let stmts2, e2 = compile_expr e2 in
      (stmts1 @ stmts2, Add (e1, e2))
  | Sub (e1, e2) ->
      let stmts1, e1 = compile_expr e1 in
      let stmts2, e2 = compile_expr e2 in
      (stmts1 @ stmts2, Sub (e1, e2))
  | Mult (e1, e2) ->
      let stmts1, e1 = compile_expr e1 in
      let stmts2, e2 = compile_expr e2 in
      (stmts1 @ stmts2, Mult (e1, e2))
  | Div (e1, e2) ->
      let stmts1, e1 = compile_expr e1 in
      let stmts2, e2 = compile_expr e2 in
      (stmts1 @ stmts2, Div (e1, e2))
  | Mod (e1, e2) ->
      let stmts1, e1 = compile_expr e1 in
      let stmts2, e2 = compile_expr e2 in
      (stmts1 @ stmts2, Mod (e1, e2))
  | Compare (op, e1, e2) ->
      let stmts1, e1 = compile_expr e1 in
      let stmts2, e2 = compile_expr e2 in
      (stmts1 @ stmts2, Compare (op, e1, e2))
  | Not e ->
      let stmts, e = compile_expr e in
      (stmts, Not e)
  | Call (func_name, args) ->
      let stmts, args = args |> List.map ~f:compile_expr |> List.unzip in
      (List.concat stmts, Call (func_name, args))
  | And _ -> failwith "TODO"
  | Or _ -> failwith "TODO"
  | If_expr _ -> failwith "TODO"

and compile_stmt = function
  | Function_def (name, params, body) ->
      [ Function_def (name, params, List.concat_map body ~f:compile_stmt) ]
  | Return expr ->
      let stmts, expr = compile_expr expr in
      stmts @ [ Return expr ]
  | Set (reg, expr) ->
      let stmts, expr = compile_expr expr in
      stmts @ [ Set (reg, expr) ]
  | Call (func_name, args) ->
      let stmts, args = args |> List.map ~f:compile_expr |> List.unzip in
      List.concat stmts @ [ Call (func_name, args) ]
  | While (cond, body) ->
      let stmts_before_cond, cond = compile_expr cond in
      stmts_before_cond
      @ [
          While (cond, List.concat_map body ~f:compile_stmt @ stmts_before_cond);
        ]
  | If _ -> failwith "TODO"

let compile program = List.concat_map program ~f:compile_stmt
