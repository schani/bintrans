open Expr
open Cond_monad
open Matcher
open Simplifiers

let rec simplify_expr fields expr =
  if is_const (cfold_expr fields expr) then
    (cm_return expr)
  else
    cm_bind (apply_to_expr_subs_with_monad cm_return cm_bind (simplify_expr fields) expr)
      (fun sexpr ->
	let rec try_simplifiers simplifiers =
	  match simplifiers with
	    { pattern = pattern ; apply = apply } :: rest ->
	      cm_if_success (match_expr fields sexpr pattern)
		(fun bindings ->
		  cm_return (apply sexpr bindings))
	        (fun _ -> (try_simplifiers rest))
	  | [] -> cm_return sexpr
	in try_simplifiers simplifiers)

let simplify_stmt fields =
  apply_to_stmt_subs_with_monad cm_return cm_bind (simplify_expr fields)
