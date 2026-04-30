open! Core
open Types

type expr =
  | Unit
  | Register of Register.t
  | Num of float
  | Bool of bool
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Mod of expr * expr
  | Compare of Compare_op.t * expr * expr
  | If_expr of { conds : (expr * expr) list; default : expr }
  | Call of Function_name.t * expr list
[@@deriving sexp]

type stmt =
  | Function_def of Function_name.t * Register.t list * stmt list
  | Return of expr
  | If of { branches : (expr * stmt list) list; else_ : stmt list }
  | Set of Register.t * expr
  | While of expr * stmt list
  | Call of Function_name.t * expr list
[@@deriving sexp]

type t = stmt list [@@deriving sexp]

(* TODO: check only has valid symbols and no keywords. should be added to parse_register_name and parse_function_name

    symbols:
      - can't start with number
      - a-z A-Z 0-9 _

    keywords (so far):
      def
      while
      set
      if
      elif
      else
      return
      true
      false
      default

 *)
let parse_register_name = function
  | Sexp.Atom name -> Register.of_string name
  | _ -> failwith "expected register name"

let parse_function_name = function
  | Sexp.Atom name -> Function_name.of_string name
  | _ -> failwith "expected function name"

let rec parse_expr = function
  | Sexp.Atom "true" -> Bool true
  | Atom "false" -> Bool false
  | Atom str -> (
      match Float.of_string_opt str with
      | Some f -> Num f
      | None -> Register (parse_register_name (Atom str)))
  | List [] -> Unit
  | List [ e1; Atom "+"; e2 ] -> Add (parse_expr e1, parse_expr e2)
  | List [ e1; Atom "-"; e2 ] -> Sub (parse_expr e1, parse_expr e2)
  | List [ e1; Atom "*"; e2 ] -> Mult (parse_expr e1, parse_expr e2)
  | List [ e1; Atom "/"; e2 ] -> Div (parse_expr e1, parse_expr e2)
  | List [ e1; Atom "&&"; e2 ] -> And (parse_expr e1, parse_expr e2)
  | List [ e1; Atom "||"; e2 ] -> Or (parse_expr e1, parse_expr e2)
  | List [ Atom "not"; e ] -> Not (parse_expr e)
  | List [ e1; Atom "%"; e2 ] -> Mod (parse_expr e1, parse_expr e2)
  | List [ e1; Atom ">"; e2 ] -> Compare (Gt, parse_expr e1, parse_expr e2)
  | List [ e1; Atom ">="; e2 ] -> Compare (Ge, parse_expr e1, parse_expr e2)
  | List [ e1; Atom "<"; e2 ] -> Compare (Lt, parse_expr e1, parse_expr e2)
  | List [ e1; Atom "<="; e2 ] -> Compare (Le, parse_expr e1, parse_expr e2)
  | List [ e1; Atom "=="; e2 ] -> Compare (Eq, parse_expr e1, parse_expr e2)
  | List [ e1; Atom "!="; e2 ] -> Compare (Ne, parse_expr e1, parse_expr e2)
  | List (Atom "??" :: cases) ->
      let rec construct_ifexpr_list rev_acc = function
        | [ Sexp.Atom "default"; expr ] -> (List.rev rev_acc, parse_expr expr)
        | cond :: expr :: rest ->
            construct_ifexpr_list
              ((parse_expr cond, parse_expr expr) :: rev_acc)
              rest
        | [] -> failwith "missing default case in ifexpr"
        | [ _ ] -> failwith "invalid ifexpr"
      in
      let conds, default = construct_ifexpr_list [] cases in
      If_expr { conds; default }
  | List (func :: args) ->
      Call (parse_function_name func, List.map ~f:parse_expr args)

let rec parse_statement = function
  | Sexp.List [ Atom "def"; func_name; List params; List body ] ->
      Function_def
        ( parse_function_name func_name,
          List.map ~f:parse_register_name params,
          List.map ~f:parse_statement body )
  | List [ Atom "return"; expr ] -> Return (parse_expr expr)
  | List (Atom "if" :: cond :: List body :: rest) ->
      let rec construct_if_list rev_acc = function
        | Sexp.Atom "elif" :: cond :: List body :: rest ->
            construct_if_list
              ((parse_expr cond, List.map ~f:parse_statement body) :: rev_acc)
              rest
        | [ Atom "else"; List body ] ->
            (List.rev rev_acc, List.map ~f:parse_statement body)
        | [] -> (List.rev rev_acc, [])
        | _ -> failwith "invalid if statement"
      in
      let first_branch = (parse_expr cond, List.map ~f:parse_statement body) in
      let branches, else_ = construct_if_list [ first_branch ] rest in
      If { branches; else_ }
  | List [ Atom "set"; register; expr ] ->
      Set (parse_register_name register, parse_expr expr)
  | List [ Atom "while"; cond; List body ] ->
      While (parse_expr cond, List.map ~f:parse_statement body)
  | List (func :: args) ->
      Call (parse_function_name func, List.map ~f:parse_expr args)
  | _ -> failwith "expected statement"

let parse_ast = List.map ~f:parse_statement
