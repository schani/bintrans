open Expr
open Matcher
open Simplifiers

let rec simplify_expr fields expr =
  if is_const (cfold_expr fields expr) then
    expr
  else
    let sexpr = apply_to_expr_subs (simplify_expr fields) expr
    in let rec try_simplifiers simplifiers =
      match simplifiers with
	{ pattern = pattern ; apply = apply } :: rest ->
	  (match match_expr fields sexpr pattern with
	    Some bindings -> apply sexpr bindings
	  | None -> try_simplifiers rest)
      | [] -> sexpr
    in try_simplifiers simplifiers

let simplify_stmt fields =
  apply_to_stmt_subs (simplify_expr fields)
