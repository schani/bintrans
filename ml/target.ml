open Int64
open List

open Expr

type binding =
    RegisterBinding of input_name * register
  | ExprBinding of input_name * expr
  | IntBinding of input_name * int64
  | FloatBinding of input_name * float
  | ConditionBinding of input_name * bool

type cost = int

type target_insn =
    { pattern : stmt_pattern ;
      matcher : stmt -> binding list -> cost option }

let binding_input_name binding =
  match binding with
    RegisterBinding (name, _) -> name
  | ExprBinding (name, _) -> name
  | IntBinding (name, _) -> name
  | FloatBinding (name, _) -> name
  | ConditionBinding (name, _) -> name

let find_binding bindings name =
  find (fun b -> (binding_input_name b) = name) bindings
