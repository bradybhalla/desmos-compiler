open! Core

module Desmos_slider_args = struct
  type t = unit [@@deriving sexp]

  let default = ()
end

module Desmos_point_args = struct
  type t = unit [@@deriving sexp]

  let default = ()
end

module Desmos_line_args = struct
  type t = unit [@@deriving sexp]

  let default = ()
end

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
  type t = { mutable cur : int; id : string; mutable has_been_reset : bool }

  (* id needs to be unique per generator or there will be repeats. It starts
     with a number to avoid conflict with user defined variables.  *)
  let create id = { cur = 0; id; has_been_reset = false }

  let generate ?(desc = "") t =
    if not t.has_been_reset then
      failwith "must reset generator before use for consistent results";
    let str =
      match desc with
      | "" -> [%string "%{t.id}_%{t.cur#Int}"]
      | desc -> [%string "%{t.id}_%{desc}_%{t.cur#Int}"]
    in
    t.cur <- t.cur + 1;
    M.of_string str

  let reset t =
    t.cur <- 0;
    t.has_been_reset <- true
end

module Register_generator = struct
  include Unique_generator (Register)

  (* make id start with 1 to avoid conflict with user defined variables *)
  let create id = create ("1" ^ id)
end

module Label_generator = Unique_generator (Label)
