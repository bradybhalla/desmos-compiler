open! Core
open Languages
open Types
open Register_stack_instrs
module Rset = Register.Set

(* greedy algorithm to do as many instructions as possible at once within a block without changing any expressions (I don't know if this is actually optimal).

  the constraints are (suppose A is an instruction that comes before B):
  - if A reads a register and B writes it, A must always stay before B
  - if A writes a register and B reads it, A must always stay before B
  - if A and B both write a register, A must always stay before B so the correct value is there in the end

  the algorithm I'm using is:
  - start with a set of all unused instructions
  - for each instruction, iterate over all instructions that come after it and remove them from the set if they cannot happen at the same time
  - everything remaining can be merged together
  - repeat until all instructions have been used

 *)

(* TODO compression: This always puts the control flow in a seperate instruction, but it should hopefully be pretty easy to change this later *)

type read_write_info = { read : Rset.t; write : Rset.t }

let intersection_is_nonempty a b = not (Set.is_empty (Set.inter a b))

let rec registers_from_expr : expr -> Rset.t = function
  | Register r -> Register.Set.singleton r
  | Num _ | Bool _ -> Register.Set.empty
  | Add (a, b)
  | Sub (a, b)
  | Mult (a, b)
  | Div (a, b)
  | And (a, b)
  | Or (a, b)
  | Mod (a, b) ->
      Set.union (registers_from_expr a) (registers_from_expr b)
  | Not e -> registers_from_expr e
  | Compare (_, a, b) ->
      Set.union (registers_from_expr a) (registers_from_expr b)
  | If_expr { conds; default } ->
      List.fold conds ~init:(registers_from_expr default)
        ~f:(fun acc (cond, expr) ->
          acc
          |> Set.union (registers_from_expr cond)
          |> Set.union (registers_from_expr expr))

let extract_read_write_registers instrs =
  let registers, generalized_sets = List.unzip instrs in
  {
    write = Rset.of_list registers;
    read =
      generalized_sets
      |> List.map ~f:(function
           | PushAndSet e | Set e -> registers_from_expr e
           | Push | Pop -> Rset.empty)
      |> Rset.union_list;
  }

let must_be_strictly_before ~a ~b =
  let a_rw = extract_read_write_registers a in
  let b_rw = extract_read_write_registers b in
  (* a needs to read before b changes the value *)
  intersection_is_nonempty a_rw.read b_rw.write
  (* a needs to write before b can read it *)
  || intersection_is_nonempty a_rw.write b_rw.read
  (* a needs to write before b so it has the correct value after b *)
  || intersection_is_nonempty a_rw.write b_rw.write

let extract_greedy_instruction stmts =
  let unwrap_yes_no = function `Yes s | `No s -> s in
  (* the helper function turns an initial list of
       [`Yes s1; `Yes s2; `Yes s3; ...]
    into something like
       [`Yes s1; `No s2; `No s3; ...]
    by iterating over the list and crossing out any statements which are not allowed yet.
   *)
  (* TODO compression: this is very inefficient because we recompute everything for each element. it would be better to just iterate through the list once and turn each statement into something like { stmts=s1; must_come_after=Rset.of_list [5;2;3] }. then as we iterate through we can check the minimim of the sets and remove indices as needed.
   *)
  let rec helper = function
    | [] -> []
    | h :: t ->
        let t' =
          List.map t ~f:(function
            | `No s -> `No s
            | `Yes s ->
                if must_be_strictly_before ~a:(unwrap_yes_no h) ~b:s then `No s
                else `Yes s)
        in
        h :: helper t'
  in
  let yes_stmts, no_stmts =
    stmts
    |> List.map ~f:(fun s -> `Yes s)
    |> helper
    |> List.partition_map ~f:(function `Yes s -> First s | `No s -> Second s)
  in
  (List.concat yes_stmts, no_stmts)

let construct_stmts stmts =
  let rec helper = function
    | [] -> []
    | stmts ->
        let stmt, remaining = extract_greedy_instruction stmts in
        stmt :: helper remaining
  in
  let strip_constructor (GeneralizedSet s) = s in
  let apply_constructor s = GeneralizedSet s in
  stmts
  |> List.map ~f:strip_constructor
  |> helper
  |> List.map ~f:apply_constructor

(* TODO: remove this if statement*)
let compile { blocks; registers } =
  if false then { blocks; registers }
  else
    {
      blocks =
        List.map blocks ~f:(fun { label; body; control_flow } ->
            { label; body = construct_stmts body; control_flow });
      registers;
    }
