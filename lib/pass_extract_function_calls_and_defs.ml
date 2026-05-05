open! Core
open! Languages
open! Types
open C_style_frontend

let register_gen = Register_generator.create "extract_function_calls"

(* iterate over (compiled) branches and either add them in parallel to the current if statement
   or create a nested if statement depening on whether there are possible side effects
   from evaluating the condition *)
let construct_short_circuited_if branches else_ =
  let rec helper branches else_ branches_acc_rev =
    match branches with
    | [] ->
        if List.is_empty branches_acc_rev then
          failwith "attempted to construct empty list";
        C_style_separated_functions.If
          { branches = List.rev branches_acc_rev; else_ }
    | ((cond_stmts, cond), body) :: rest -> (
        match cond_stmts with
        | [] -> helper rest else_ ((cond, body) :: branches_acc_rev)
        | cond_stmts ->
            C_style_separated_functions.If
              {
                branches = List.rev branches_acc_rev;
                else_ = cond_stmts @ [ helper rest else_ [ (cond, body) ] ];
              })
  in
  match branches with
  | [] -> failwith "if statement with no conditions is not allowed"
  | ((cond_stmts, cond), body) :: rest ->
      (* statements from first branch go in the toplevel *)
      cond_stmts @ [ helper rest else_ [ (cond, body) ] ]

let rec compile_expr = function
  | Unit -> ([], C_style_separated_functions.Unit)
  | Register reg -> ([], Register reg)
  | Num n -> ([], Num n)
  | Bool b -> ([], Bool b)
  | Add (e1, e2) ->
      let calls1, e1 = compile_expr e1 in
      let calls2, e2 = compile_expr e2 in
      (calls1 @ calls2, Add (e1, e2))
  | Sub (e1, e2) ->
      let calls1, e1 = compile_expr e1 in
      let calls2, e2 = compile_expr e2 in
      (calls1 @ calls2, Sub (e1, e2))
  | Mult (e1, e2) ->
      let calls1, e1 = compile_expr e1 in
      let calls2, e2 = compile_expr e2 in
      (calls1 @ calls2, Mult (e1, e2))
  | Div (e1, e2) ->
      let calls1, e1 = compile_expr e1 in
      let calls2, e2 = compile_expr e2 in
      (calls1 @ calls2, Div (e1, e2))
  | Mod (e1, e2) ->
      let calls1, e1 = compile_expr e1 in
      let calls2, e2 = compile_expr e2 in
      (calls1 @ calls2, Mod (e1, e2))
  | Compare (op, e1, e2) ->
      let calls1, e1 = compile_expr e1 in
      let calls2, e2 = compile_expr e2 in
      (calls1 @ calls2, Compare (op, e1, e2))
  | Not e ->
      let calls, e = compile_expr e in
      (calls, Not e)
  | And (e1, e2) -> (
      let stmts1, e1' = compile_expr e1 in
      let stmts2, e2' = compile_expr e2 in
      match stmts2 with
      | [] -> (stmts1, And (e1', e2'))
      | _ ->
          (* e1 and e2 == if e1 then e2 else false *)
          let store_register =
            Register_generator.generate ~desc:"and" register_gen
          in
          ( C_style_separated_functions.Decl store_register
            :: construct_short_circuited_if
                 [ ((stmts1, e1'), stmts2 @ [ Set (store_register, e2') ]) ]
                 [ Set (store_register, Bool false) ],
            Register store_register ))
  | Or (e1, e2) -> (
      let stmts1, e1' = compile_expr e1 in
      let stmts2, e2' = compile_expr e2 in
      match stmts2 with
      | [] -> (stmts1, Or (e1', e2'))
      | _ ->
          (* e1 or e2 == if e1 then true else e2 *)
          let store_register =
            Register_generator.generate ~desc:"or" register_gen
          in
          ( C_style_separated_functions.Decl store_register
            :: construct_short_circuited_if
                 [ ((stmts1, e1'), [ Set (store_register, Bool true) ]) ]
                 (stmts2 @ [ Set (store_register, e2') ]),
            Register store_register ))
  | If_expr { conds; default } ->
      let conds' =
        List.map conds ~f:(fun (cond, value) ->
            (compile_expr cond, compile_expr value))
      in
      let defualt_stmts, default = compile_expr default in
      if
        List.for_all conds' ~f:(fun ((cond_stmts, _), (value_stmts, _)) ->
            List.is_empty cond_stmts && List.is_empty value_stmts)
        && List.is_empty defualt_stmts
      then
        (* if there are no side-effects we can keep the same form *)
        ( [],
          If_expr
            {
              conds =
                List.map conds' ~f:(fun ((_, cond), (_, value)) ->
                    (cond, value));
              default;
            } )
      else
        (* statements or expressions have side-effects so need to turn it
          into an if statement for proper short-circuiting *)
        let store_register =
          Register_generator.generate ~desc:"ifexpr" register_gen
        in
        let branches =
          List.map conds' ~f:(fun (cond, (value_stmts, value)) ->
              (cond, value_stmts @ [ Set (store_register, value) ]))
        in
        let else_ = defualt_stmts @ [ Set (store_register, default) ] in
        ( C_style_separated_functions.Decl store_register
          :: construct_short_circuited_if branches else_,
          Register store_register )
  | Call (func_name, args) ->
      let extracted_calls, args =
        args |> List.map ~f:compile_expr |> List.unzip
      in
      let extracted_calls = List.concat extracted_calls in
      let store_register =
        Register_generator.generate ~desc:"call" register_gen
      in
      ( extracted_calls
        @ [
            C_style_separated_functions.Decl store_register;
            C_style_separated_functions.Call
              { func_name; args; ret = Some store_register };
          ],
        Register store_register )

and compile_expr_enforce_no_extracted_calls expr =
  let calls, expr = compile_expr expr in
  if List.length calls > 0 then
    failwith "Call expression found where it should not have been allowed";
  expr

(* Compile normal statements, ignoring function definitions *)
and compile_stmt = function
  | Function_def _ -> []
  | Return expr ->
      let extracted_calls, expr = compile_expr expr in
      extracted_calls @ [ C_style_separated_functions.Return expr ]
  | If { branches; else_ } ->
      let branches =
        List.map branches ~f:(fun (cond, body) ->
            (compile_expr cond, List.concat_map ~f:compile_stmt body))
      in
      let else_ = List.concat_map else_ ~f:compile_stmt in
      construct_short_circuited_if branches else_
  | Set (reg, expr) ->
      (* TODO: if expr is a function call then we have a speical case where we can output a call directly to the register. right now it adds an unnecessary extra step. *)
      let extracted_calls, expr = compile_expr expr in
      extracted_calls @ [ C_style_separated_functions.Set (reg, expr) ]
  | While (cond, stmts) ->
      (* we need to run stmts_before_cond both before the loop and at the end of the loop body *)
      let stmts_before_cond, cond = compile_expr cond in
      let non_decl_stmts =
        List.filter stmts_before_cond ~f:(function
          | C_style_separated_functions.Decl _ -> false
          | _ -> true)
      in
      stmts_before_cond
      @ [
          While
            {
              cond;
              body = List.concat_map ~f:compile_stmt stmts @ non_decl_stmts;
            };
        ]
  | Decl reg -> [ C_style_separated_functions.Decl reg ]
  | Call (func_name, args) ->
      let extracted_calls, args =
        args |> List.map ~f:compile_expr |> List.unzip
      in
      let extracted_calls = List.concat extracted_calls in
      extracted_calls
      @ [ C_style_separated_functions.Call { func_name; args; ret = None } ]

let extract_and_compile_function_defs = function
  | Function_def (name, params, stmts) ->
      Some
        ( name,
          C_style_separated_functions.
            { params; body = List.concat_map ~f:compile_stmt stmts } )
  | Return _ | If _ | Set (_, _) | Decl _ | While (_, _) | Call (_, _) -> None

let compile
    ({ C_style_frontend.stmts = program; info = `Checked_function_defs } :
      [ `Checked_function_defs ] C_style_frontend.t) =
  Register_generator.reset register_gen;
  let functions =
    program
    |> List.filter_map ~f:extract_and_compile_function_defs
    |> Function_name.Map.of_alist_exn
  in
  {
    C_style_separated_functions.functions;
    main = List.concat_map ~f:compile_stmt program;
    info = `Unchecked;
  }
