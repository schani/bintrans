open Int64
open List

open Cond_monad
open Expr
open Matcher
open Pruner
open Simplify

(*** simplifying expressions and statements ***)

let rec simplify_and_prune_until_fixpoint depth print prune simplify fields x =
  if depth >= 100 then
    (print_int depth ; print_string " : " ; print x ; print_newline ())
  else
    () ;
  cm_bind (prune fields x)
    (fun px ->
      cm_bind (simplify fields px)
	(fun spx ->
	  if spx = x then
	    cm_return x
	  else
	    simplify_and_prune_until_fixpoint (depth + 1) print prune simplify fields spx))

let simplify_and_prune_expr_until_fixpoint =
  simplify_and_prune_until_fixpoint 0 print_expr (fun f x -> prune_expr f x minus_one) simplify_expr

let simplify_and_prune_stmt_until_fixpoint =
  simplify_and_prune_until_fixpoint 0 (fun _ -> ()) prune_stmt simplify_stmt

(*** simplifying the conditions ***)

let rec uniq lst =
  match lst with
      [] -> []
    | x :: xs ->
	let uxs = uniq xs
	in if mem x uxs then
	    uxs
	  else
	    x :: uxs

let simplify_conditions conds =
  let prune_and_simplify x =
    fst (cm_yield (simplify_and_prune_expr_until_fixpoint [] x))
  in uniq (map (fun x -> cfold_expr [] (prune_and_simplify x))
	     (filter (fun x -> not (is_const (cfold_expr [] x))) conds))

let fast_simplify_conditions conds =
  map (cfold_expr [])
    (filter (fun x -> not (is_const (cfold_expr [] x))) conds)

(*** exploring all field values ***)

type stmt_form_match =
    { match_data : match_data ;
      match_conditions : expr list ;
      best_sub_matches : (expr * expr_match) list }

type stmt_form =
    { stmt : stmt ;
      form_conditions : expr list ;
      matches : stmt_form_match list }

let explore_all_fields stmt fields target_insns =
  let rec add_stmt_form_match fields stmt matches =
    match matches with
	[] ->
	  let (stmt_match, best_sub_matches, conditions) = recursively_match_stmt fields stmt target_insns
	  in [{ match_data = stmt_match.stmt_match_data ;
		match_conditions = conditions ;
		best_sub_matches = best_sub_matches }]
      | { match_data = match_data ;
	  match_conditions = conditions ;
	  best_sub_matches = best_sub_matches } as stmt_form_match :: rest_matches ->
	  if for_all (fun c ->
			match cfold_expr fields c with
			    ConditionConst true -> true
			  | _ -> false) conditions then
	    matches
	  else
	    stmt_form_match :: (add_stmt_form_match fields stmt rest_matches)
  and add_stmt_form fields stmt_forms =
    match stmt_forms with
	[] ->
	  let (new_stmt, new_conds) = cm_yield (simplify_and_prune_stmt_until_fixpoint fields stmt)
	  in [{ stmt = new_stmt ;
		form_conditions = fast_simplify_conditions new_conds ;
		matches = add_stmt_form_match fields new_stmt [] }]
      | { stmt = stmt ;
	  form_conditions = conds ;
	  matches = matches } as stmt_form :: rest_stmt_forms ->
	  if for_all (fun c ->
			match cfold_expr fields c with
			    ConditionConst true -> true
			  | _ -> false) conds then
	    { stmt = stmt ;
	      form_conditions = conds ;
	      matches = add_stmt_form_match fields stmt matches } :: rest_stmt_forms
	  else
	    stmt_form :: (add_stmt_form fields rest_stmt_forms)
  and explore fields_so_far rest_fields stmts_so_far =
    match rest_fields with
      [] ->
	add_stmt_form fields_so_far stmts_so_far
    | (name, min, max) :: rest_fields when (compare min max) < 0 ->
	let stmts_so_far = explore ((name, min) :: fields_so_far) rest_fields stmts_so_far
	in explore fields_so_far ((name, (add min one), max) :: rest_fields) stmts_so_far
    | _ -> stmts_so_far
  in
    explore [] fields []

