open! Core
open Languages
open Types
open C_style_frontend

(* TODO brady: rename "plotting" to "desmos" *)

let rec expr_contains_call = function
  | Unit | Register _ | Num _ | Bool _ -> false
  | Add (a, b)
  | Sub (a, b)
  | Mult (a, b)
  | Div (a, b)
  | And (a, b)
  | Or (a, b)
  | Mod (a, b)
  | Compare (_, a, b) ->
      expr_contains_call a || expr_contains_call b
  | Not e -> expr_contains_call e
  | If_expr { conds; default } ->
      List.exists conds ~f:(fun (c, e) ->
          expr_contains_call c || expr_contains_call e)
      || expr_contains_call default
  | Call (_, _) -> true

let check_no_calls_in_expr expr =
  if expr_contains_call expr then
    error_s
      [%message "function calls not allowed in desmos plotting expressions"]
  else Ok ()

let extract_function_defs stmts =
  let open Or_error.Let_syntax in
  let rec check_nontoplevel ~returns_allowed = function
    | [] -> Ok ()
    | stmt :: rest -> (
        match stmt with
        | Function_def (name, _, _) ->
            error_s
              [%message
                "all functions must be at the toplevel" (name : Function_name.t)]
        | Return _ ->
            if not returns_allowed then
              error_s [%message "returns only allowed inside a function"]
            else if not (List.is_empty rest) then
              error_s [%message "unreachable code after return"]
            else check_nontoplevel ~returns_allowed rest
        | If { branches; else_ } ->
            let%bind () =
              List.map branches ~f:(fun (_, body) ->
                  check_nontoplevel ~returns_allowed body)
              |> Or_error.combine_errors_unit
            in
            let%bind () = check_nontoplevel ~returns_allowed else_ in
            check_nontoplevel ~returns_allowed rest
        | While (_, body) ->
            let%bind () = check_nontoplevel ~returns_allowed body in
            check_nontoplevel ~returns_allowed rest
        | Set (_, _) | Call (_, _) | Decl _ ->
            check_nontoplevel ~returns_allowed rest
        | Desmos_decl _ | Desmos_point _ | Desmos_line _ ->
            error_s [%message "desmos statements must be at the toplevel"])
  in
  let check_toplevel = function
    | Function_def (name, params, body) ->
        let%bind () = check_nontoplevel ~returns_allowed:true body in
        Ok [ (name, params, body) ]
    | Desmos_decl _ -> Ok []
    | Desmos_point { x; y; args = _ } ->
        let%bind () = check_no_calls_in_expr x in
        let%bind () = check_no_calls_in_expr y in
        Ok []
    | Desmos_line { x1; y1; x2; y2; args = _ } ->
        let%bind () = check_no_calls_in_expr x1 in
        let%bind () = check_no_calls_in_expr y1 in
        let%bind () = check_no_calls_in_expr x2 in
        let%bind () = check_no_calls_in_expr y2 in
        Ok []
    | stmt ->
        let%bind () = check_nontoplevel ~returns_allowed:false [ stmt ] in
        Ok []
  in
  List.map ~f:check_toplevel stmts
  |> Or_error.combine_errors
  |> Or_error.map ~f:List.concat

let check_function_returns func_name stmts =
  let open Or_error.Let_syntax in
  (* boolean represents if all branches end in a return *)
  let rec helper = function
    | [] -> Ok false
    | stmt :: rest -> (
        match stmt with
        | Desmos_decl _ | Desmos_point _ | Desmos_line _ | Function_def (_, _, _)
          ->
            failwith
              "should have been caught already, so there is a bug in the \
               compiler"
        | Return _ -> Ok true
        | If { branches; else_ } ->
            let%bind all_ifs_return =
              List.map branches ~f:(fun (_, body) -> helper body)
              |> Or_error.combine_errors
              |> Or_error.map ~f:(List.for_all ~f:Fun.id)
            in
            let%bind else_returns = helper else_ in
            if all_ifs_return && else_returns then Ok true else helper rest
        | While (_, body) ->
            let%bind _ = helper body in
            helper rest
        | Set (_, _) | Call (_, _) | Decl _ -> helper rest)
  in
  if%bind helper stmts then Ok ()
  else
    error_s [%message "function missing return" (func_name : Function_name.t)]

let compile { stmts; status = `Unchecked } =
  let open Or_error.Let_syntax in
  (* get function definitions and make sure they are only in the toplevel. also
     make sure there are no returns in the toplevel. *)
  let%bind func_defs = extract_function_defs stmts in
  (* make sure all function names are unique *)
  let%bind () =
    match
      List.find_a_dup func_defs ~compare:(fun (a, _, _) (b, _, _) ->
          Function_name.compare a b)
    with
    | Some (dup, _, _) ->
        error_s [%message "duplicate function" (dup : Function_name.t)]
    | None -> Ok ()
  in
  (* make sure all paramter names are unique *)
  let%bind () =
    List.map func_defs ~f:(fun (func_name, params, _) ->
        match List.find_a_dup params ~compare:Register.compare with
        | Some dup ->
            error_s
              [%message
                "duplicate parameter"
                  (dup : Register.t)
                  (func_name : Function_name.t)]
        | None -> Ok ())
    |> Or_error.combine_errors_unit
  in
  (* check that every function always returns and doesn't have unreachable code after return *)
  let%bind () =
    List.map func_defs ~f:(fun (name, _, body) ->
        check_function_returns name body)
    |> Or_error.combine_errors_unit
  in
  Ok { stmts; status = `Valid }
