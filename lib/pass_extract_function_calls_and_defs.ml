open! Core
open! Languages
open! Types
open C_style_frontend

(* TODO: should probably encapsulate this in a module *)
let next_register_id = ref 0

let generate_register () =
  let res = [%string "0internal_%{!next_register_id#Int}"] in
  next_register_id := !next_register_id + 1;
  Register.of_string res

let reset_register_generator () = next_register_id := 0

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
  | And (e1, e2) ->
      let e1 = compile_expr_enforce_no_extracted_calls e1 in
      let e2 = compile_expr_enforce_no_extracted_calls e2 in
      ([], And (e1, e2))
  | Or (e1, e2) ->
      let e1 = compile_expr_enforce_no_extracted_calls e1 in
      let e2 = compile_expr_enforce_no_extracted_calls e2 in
      ([], Or (e1, e2))
  | If_expr { conds; default } ->
      ( [],
        If_expr
          {
            conds =
              List.map conds ~f:(fun (expr, result) ->
                  ( compile_expr_enforce_no_extracted_calls expr,
                    compile_expr_enforce_no_extracted_calls result ));
            default = compile_expr_enforce_no_extracted_calls default;
          } )
  | Call (func_name, args) ->
      let extracted_calls, args =
        args |> List.map ~f:compile_expr |> List.unzip
      in
      let extracted_calls = List.concat extracted_calls in
      let store_register = generate_register () in
      ( extracted_calls
        @ [
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
let rec compile_stmt = function
  | Function_def _ -> []
  | Return expr ->
      let extracted_calls, expr = compile_expr expr in
      extracted_calls @ [ C_style_separated_functions.Return expr ]
  | If { branches = []; _ } -> failwith "empty if statement shouldn't exist"
  | If { branches = first_branch :: other_branches; else_ } ->
      (* TODO: once short-circuiting behavior has been fully decided this may need to be updated.
         Right now this assumes that simultaneous conditionals do short circuit like a normal
         language. *)
      let transform_branch ~allow_extracted_calls (cond, body) =
        let compiled_body = List.concat_map ~f:compile_stmt body in
        if allow_extracted_calls then
          let extracted_calls, cond = compile_expr cond in
          (extracted_calls, (cond, compiled_body))
        else
          let cond = compile_expr_enforce_no_extracted_calls cond in
          ([], (cond, compiled_body))
      in
      (* It is possible there is a function call in the first conditional,
         but after the previous pass there should not be any function call
         in other conditionals. We check this here to catch any possible
         errors. *)
      let extracted_calls, first_branch =
        transform_branch ~allow_extracted_calls:true first_branch
      in
      let other_branches =
        other_branches
        |> List.map ~f:(transform_branch ~allow_extracted_calls:false)
        |> List.map ~f:snd
      in
      extracted_calls
      @ [
          C_style_separated_functions.If
            {
              branches = first_branch :: other_branches;
              else_ = List.concat_map ~f:compile_stmt else_;
            };
        ]
  | Set (reg, expr) ->
      (* TODO: if expr is a function call then we have a speical case where we can output a call directly to the register. right now it adds an unnecessary extra step. *)
      let extracted_calls, expr = compile_expr expr in
      extracted_calls @ [ C_style_separated_functions.Set (reg, expr) ]
  | While (cond, stmts) ->
      [
        While
          {
            cond = compile_expr cond;
            body = List.concat_map ~f:compile_stmt stmts;
          };
      ]
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
  | Return _ | If _ | Set (_, _) | While (_, _) | Call (_, _) -> None

let compile program =
  reset_register_generator ();
  let functions =
    program
    |> List.filter_map ~f:extract_and_compile_function_defs
    |> Function_name.Map.of_alist_exn
  in
  {
    C_style_separated_functions.functions;
    main = List.concat_map ~f:compile_stmt program;
  }
