(*
 * explorer.ml
 *
 * bintrans
 *
 * Copyright (C) 2004 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)

open Int64
open List

open Monad
open Utils
open Cond_monad
open Expr
open Matcher
open Pruner
open Simplify
open Mapping

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

let simplify_and_prune_expr_until_fixpoint mapping =
  simplify_and_prune_until_fixpoint 0 print_expr (fun f x -> prune_expr mapping f x minus_one) simplify_expr

let simplify_and_prune_stmt_until_fixpoint mapping =
  simplify_and_prune_until_fixpoint 0 (fun _ -> ()) (prune_stmt mapping) simplify_stmt

(*** simplifying the conditions ***)

let simplify_conditions conds =
  let prune_and_simplify x =
    fst (cm_yield (simplify_and_prune_expr_until_fixpoint dummy_mapping [] x))
  in uniq (map (fun x -> cfold_expr [] (prune_and_simplify x))
	     (filter (fun x -> not (is_const (cfold_expr [] x))) conds))

let fast_simplify_conditions conds =
  map (cfold_expr [])
    (filter (fun x -> not (is_const (cfold_expr [] x))) conds)

(*** applying the sex optimization ***)

let rec sex_simplify_expr mapping fields expr =
  let optimize_and_simplify one expr =
    let mapping = make_sex_mapping mapping one
    in cm_bind (optimize_sex_expr mapping fields expr)
	 (fun expr ->
	    simplify_and_prune_expr_until_fixpoint mapping fields expr)
  in if is_const (cfold_expr fields expr) then
      cm_return expr
    else
      cm_bind (apply_to_expr_subs_with_monad cm_return cm_bind (sex_simplify_expr mapping fields) expr)
	(fun expr ->
	   (make_bind2 cm_bind cm_bind)
	     (optimize_and_simplify false expr)
	     (optimize_and_simplify true expr)
	     (fun expr0 expr1 ->
		if expr0 = expr1 then
		  cm_return expr0
		else
		  ( (* print_expr expr0 ; print_newline () ; print_expr expr1 ; print_newline () ; *)
		  cm_return expr)))

let sex_simplify_stmt mapping fields stmt =
  let optimize_and_simplify one stmt =
    let mapping = make_sex_mapping mapping one
    in cm_bind (optimize_sex_stmt mapping fields stmt)
	 (fun stmt ->
	    simplify_and_prune_stmt_until_fixpoint mapping fields stmt)
  in cm_bind (apply_to_stmt_subs_with_monad cm_return cm_bind (sex_simplify_expr mapping fields) stmt)
       (fun sstmt ->
	  if stmt <> sstmt then
	    ( (* print_stmt stmt ; print_stmt sstmt *) )
	  else
	    () ;
	  (make_bind2 cm_bind cm_bind)
	    (optimize_and_simplify false sstmt)
	    (optimize_and_simplify true sstmt)
            (fun stmt0 stmt1 ->
	       if stmt0 = stmt1 then
		 (if sstmt <> stmt0 then
		    ( (* print_stmt sstmt ; print_stmt stmt0 *) )
		  else
		    () ;
		  cm_return stmt0)
	       else
		 cm_return sstmt))

(*** exploring all field values ***)

type stmt_form_match =
    { match_datas : match_data list ;
      match_conditions : expr list ;
      best_sub_matches : (expr * expr_match list) list }

type stmt_form =
    { stmt : stmt ;
      form_conditions : expr list ;
      matches : stmt_form_match list }

let explore_all_fields stmt mapping fields target_insns =
  let rec add_stmt_form_match fields stmt matches =
    match matches with
	[] ->
	  let (stmt_matches, best_sub_matches, conditions) = recursively_match_stmt fields stmt target_insns
	  in [{ match_datas = map (fun m -> m.stmt_match_data) stmt_matches ;
		match_conditions = simplify_conditions conditions ;
		best_sub_matches = best_sub_matches }]
      | { match_datas = match_datas ;
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
	  let (new_stmt, new_conds) =
	    cm_yield
	      (cm_bind (simplify_and_prune_stmt_until_fixpoint mapping fields stmt)
		 (fun sstmt -> sex_simplify_stmt mapping fields sstmt))
	  in [{ stmt = new_stmt ;
		form_conditions = simplify_conditions new_conds ;
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
