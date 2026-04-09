open! Core
open! Languages
open! Types
module Rset = Register.Set

(* TODO brady: 
  - move liveness analysis here
  - main reason is to know which registers need to be saved at a "call"
  - could also be useful for letting us reuse registers later on in a block of code.
    for example, we could rename a functions parameters to use args from the caller
    code that doesn't need it anymore
*)
