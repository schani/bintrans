open Expr
open Matcher
open Simplifiers

let rec simplify_expr expr =
  let sexpr = apply_to_expr_subs simplify_expr expr
  in let rec try_simplifiers simplifiers =
    match simplifiers with
      { pattern = pattern ; apply = apply } :: rest ->
	(match match_expr sexpr pattern with
	  Some bindings -> apply sexpr bindings
	| None -> try_simplifiers rest)
    | [] -> sexpr
  in try_simplifiers simplifiers

let simplify_stmt =
  apply_to_stmt_subs simplify_expr
