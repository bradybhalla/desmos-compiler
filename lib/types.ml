open! Core

module Register =
  String_id.Make
    (struct
      let module_name = "Register"
    end)
    ()

module Label =
  String_id.Make
    (struct
      let module_name = "Label"
    end)
    ()

module Function_name =
  String_id.Make
    (struct
      let module_name = "Function_name"
    end)
    ()

module Compare_op = struct
  type t = Eq | Lt | Gt | Ge | Le | Ne [@@deriving sexp]
end

module Unique_generator (M : String_id.S) = struct
  type t = { cur : int ref; id : string }

  (* id needs to be unique per generator or there will be repeats  *)
  let create id = { cur = ref 0; id }

  let generate ?(desc = "") { cur; id } =
    let str =
      match desc with
      | "" -> [%string "%{id}_%{!cur#Int}"]
      | desc -> [%string "%{id}_%{!cur#Int}_%{desc}"]
    in
    cur := !cur + 1;
    M.of_string str

  let reset { cur; id = _ } = cur := 0
end

module Register_generator = Unique_generator (Register)
module Label_generator = Unique_generator (Label)
