open! Core
open Languages
open Types

(* make sure vars are only used after being declared and that all varaibles are in scope.  *)

let rec check_expr ~ensure_in_scope =
  let open Or_error.Let_syntax in
  function
  | C_style_separated_functions.Unit -> Ok ()
  | Num _ -> Ok ()
  | Bool _ -> Ok ()
  | Register r -> ensure_in_scope r
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mult (e1, e2)
  | Div (e1, e2)
  | And (e1, e2)
  | Or (e1, e2)
  | Mod (e1, e2)
  | Compare (_, e1, e2) ->
      let%bind () = check_expr ~ensure_in_scope e1 in
      check_expr ~ensure_in_scope e2
  | Not e -> check_expr ~ensure_in_scope e
  | If_expr { conds; default } ->
      let%bind () =
        conds
        |> List.map ~f:(fun (cond, value) ->
               let%bind () = check_expr ~ensure_in_scope cond in
               check_expr ~ensure_in_scope value)
        |> Or_error.combine_errors_unit
      in
      check_expr ~ensure_in_scope default

let check_stmts ~global_vars ~allow_shadow_globals ~parent_vars =
  let open Or_error.Let_syntax in
  let rec helper ~parent_vars ~cur_vars =
    let ensure_in_scope reg =
      if
        Set.mem cur_vars reg || Set.mem parent_vars reg
        || Set.mem global_vars reg
      then Ok ()
      else error_s [%sexp "variable not declared in scope", (reg : Register.t)]
    in
    let child_scope_parent_vars = Set.union parent_vars cur_vars in
    function
    | [] -> Ok cur_vars
    | stmt :: rest -> (
        match stmt with
        | C_style_separated_functions.Decl reg ->
            if
              Set.mem cur_vars reg
              || ((not allow_shadow_globals) && Set.mem global_vars reg)
            then
              error_s
                [%sexp
                  "variable already declared in this scope", (reg : Register.t)]
            else helper ~parent_vars ~cur_vars:(Set.add cur_vars reg) rest
        | If { branches; else_ } ->
            let%bind () =
              List.map branches ~f:(fun (cond, body) ->
                  let%bind () = check_expr ~ensure_in_scope cond in
                  helper ~parent_vars:child_scope_parent_vars
                    ~cur_vars:Register.Set.empty body
                  |> Or_error.ignore_m)
              |> Or_error.combine_errors_unit
            in
            let%bind () =
              helper ~parent_vars:child_scope_parent_vars
                ~cur_vars:Register.Set.empty else_
              |> Or_error.ignore_m
            in
            helper ~parent_vars ~cur_vars rest
        | While { cond; body } ->
            let%bind () = check_expr ~ensure_in_scope cond in
            let%bind () =
              helper ~parent_vars:child_scope_parent_vars
                ~cur_vars:Register.Set.empty body
              |> Or_error.ignore_m
            in
            helper ~parent_vars ~cur_vars rest
        | Call { func_name = _; args; ret } ->
            let%bind () =
              Option.value_map ret ~default:(Ok ()) ~f:ensure_in_scope
            in
            let%bind () =
              args
              |> List.map ~f:(check_expr ~ensure_in_scope)
              |> Or_error.combine_errors_unit
            in
            helper ~parent_vars ~cur_vars rest
        | Return e ->
            let%bind () = check_expr ~ensure_in_scope e in
            helper ~parent_vars ~cur_vars rest
        | Set (r, e) ->
            let%bind () = ensure_in_scope r in
            let%bind () = check_expr ~ensure_in_scope e in
            helper ~parent_vars ~cur_vars rest)
  in
  helper ~parent_vars ~cur_vars:Register.Set.empty

let check_desmos_plot ~global_vars =
  let open Or_error.Let_syntax in
  let ensure_in_scope reg =
    if Set.mem global_vars reg then Ok ()
    else error_s [%sexp "variable not declared in scope", (reg : Register.t)]
  in
  function
  | C_style_separated_functions.Point { x; y; args = _ } ->
      let%bind () = check_expr ~ensure_in_scope x in
      check_expr ~ensure_in_scope y
  | Line { x1; y1; x2; y2; args = _ } ->
      let%bind () = check_expr ~ensure_in_scope x1 in
      let%bind () = check_expr ~ensure_in_scope y1 in
      let%bind () = check_expr ~ensure_in_scope x2 in
      check_expr ~ensure_in_scope y2

let compile
    {
      C_style_separated_functions.functions;
      main;
      desmos_decls;
      desmos_plot;
      status = `Unchecked;
    } =
  let open Or_error.Let_syntax in
  let desmos_decl_vars =
    List.map desmos_decls
      ~f:(fun (d : C_style_separated_functions.desmos_decl) -> d.reg)
    |> Register.Set.of_list
  in
  let%bind global_vars =
    check_stmts ~global_vars:desmos_decl_vars ~allow_shadow_globals:false
      ~parent_vars:Register.Set.empty main
  in
  let global_vars = Set.union global_vars desmos_decl_vars in
  let%bind () =
    functions |> Map.to_alist
    |> List.map ~f:(fun (_, { params; body }) ->
           check_stmts ~global_vars ~allow_shadow_globals:true
             ~parent_vars:(Register.Set.of_list params)
             body
           |> Or_error.ignore_m)
    |> Or_error.combine_errors_unit
  in
  let%bind () =
    desmos_plot
    |> List.map ~f:(check_desmos_plot ~global_vars)
    |> Or_error.combine_errors_unit
  in
  Ok
    {
      C_style_separated_functions.functions;
      main;
      desmos_decls;
      desmos_plot;
      status = `Valid;
    }
