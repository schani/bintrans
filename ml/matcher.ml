(*
 * matcher.ml
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

open List
open Int64

open Monad
open Expr
open Cond_monad
open Uncertainty

exception No_match
exception Wrong_binding
exception Wrong_pattern

(*** bindings ***)

type binding =
    RegisterBinding of input_name * register
  | ExprBinding of input_name * expr
  | ConstBinding of input_name * expr
  | WidthBinding of input_name * int

let binding_input_name binding =
  match binding with
    RegisterBinding (name, _) -> name
  | ExprBinding (name, _) -> name
  | ConstBinding (name, _) -> name
  | WidthBinding (name, _) -> name

let find_binding bindings name =
  find (fun b -> (binding_input_name b) = name) bindings

let get_register_binding bindings alloc name =
  match find_binding bindings name with
      RegisterBinding (_, register) -> register
    | ExprBinding (_, expr) ->
	(try
	   assoc expr alloc ;
	   IntermediateRegister expr
	 with
	     Not_found -> raise Wrong_binding)
    | _ -> raise Wrong_binding

let get_const_binding bindings name =
  match find_binding bindings name with
      ConstBinding (_, expr) -> expr
    | _ -> raise Wrong_binding

let get_int_const_binding fields bindings name =
  match cfold_expr fields (get_const_binding bindings name) with
      IntConst (IntLiteral const) -> const
    | _ -> raise Expression_not_const

let get_width_binding bindings name =
  match find_binding bindings name with
      WidthBinding (_, width) -> width
    | _ -> raise Wrong_binding

(*** target insns ***)

type cost = int uncertainty

type target_insn =
    { name : string ;
      pattern : stmt_pattern ;
      matcher : (input_name * int64) list -> binding list -> cost condition_monad }

(*** matches ***)

type match_data =
    { target_insn : target_insn ;
      cost : cost ;
      cumulative_cost : cost ;
      bindings : binding list }

type expr_match =
    { matched_expr : expr ;
      expr_match_data : match_data }

type stmt_match =
    { matched_stmt : stmt ;
      stmt_match_data : match_data }

(* inspecting *)

let better_expr_match m1 m2 =
  if m1.expr_match_data.cumulative_cost < m2.expr_match_data.cumulative_cost then
    m1
  else
    m2

let better_stmt_match m1 m2 =
  if m1.stmt_match_data.cumulative_cost < m2.stmt_match_data.cumulative_cost then
    m1
  else
    m2

(* printing *)

let rec print_whole_match stmt_match best_matches_alist =
  let print_binding binding =
    match binding with
	RegisterBinding (_, register) -> print_register register
      | ExprBinding (_, expr) | ConstBinding (_, expr) -> print_expr expr
      | WidthBinding (_, width) -> print_int width
  in let print_bindings bindings =
    iter (fun binding ->
	    print_string (binding_input_name binding) ; print_string ": " ; print_binding binding ; print_newline ())
      bindings
  in print_string "matched stmt: " ; print_stmt stmt_match.matched_stmt ;
  print_string "target pattern: " ; print_stmt_pattern stmt_match.stmt_match_data.target_insn.pattern ;
  print_string "cost: " ; print_uncert print_int stmt_match.stmt_match_data.cumulative_cost ; print_newline () ;
  print_string "bindings: " ; print_bindings stmt_match.stmt_match_data.bindings

let print_matches_alist alist =
  iter (fun (expr, mtch) ->
          print_expr expr ; print_string " -> (" ; print_uncert print_int mtch.expr_match_data.cumulative_cost ; print_string ") " ; print_stmt_pattern mtch.expr_match_data.target_insn.pattern)
    alist ;
  print_newline ()

(*** bindings ***)

let combine_maybe combinator m1 m2 =
  match (m1, m2) with
    (Some v1, Some v2) -> Some (combinator v1 v2)
  | _ -> None

let combine_bindings cm1 cm2 =
  (make_bind2 cm_bind cm_bind) cm1 cm2
    (fun b1 b2 -> cm_return (b1 @ b2))

(*** matching ***)

let make_some x = Some x

let match_expr_generic register_const_func fields expr pattern =
  let return = cm_return
  and fail = cm_fail
  and when_cfold = cm_when fields
  in let rec match_int_const int_const pattern =
      match (int_const, pattern) with
	  (IntLiteral const, AnyInt input_name) -> return [ ConstBinding (input_name, IntConst int_const) ]
	| (IntLiteral const1, TheInt const2) ->
	    when_cfold (Binary (IntEqual, IntConst int_const, const2))
	      (fun _ -> return [])
	| _ -> raise Expression_not_const
     and match_expr_int_const expr cexpr pattern =
      match (cexpr, pattern) with
	  (_, AnyInt input_name) when is_register_const fields expr ->
	    return [ ConstBinding (input_name, expr) ]
	| (IntConst (IntLiteral const1), TheInt const2) ->
	    when_cfold (Binary (IntEqual, expr, const2))
	      (fun _ -> (return []))
	| (_, TheInt const2) when is_register_const fields expr ->
	    register_const_func fields expr const2
	| _ ->
	    fail
     and match_rec expr pattern =
      let cexpr = cfold_expr fields expr
      in match (cexpr, pattern) with
	  (_, IntPattern int_const_pattern) ->
	    match_expr_int_const expr cexpr int_const_pattern
	| (FloatConst const, FloatPattern (AnyFloat input_name)) ->
	    return [ ConstBinding (input_name, expr) ]
	| (FloatConst const1, FloatPattern (TheFloat const2)) ->
	    when_cfold (Binary (FloatEqual, expr, FloatConst const2))
	      (fun _ -> (return []))
	| (ConditionConst const, ConditionPattern (AnyBool input_name)) ->
	    return [ ConstBinding (input_name, expr) ]
	| (ConditionConst const1, ConditionPattern (TheBool const2)) ->
	    when_cfold (Unary (ConditionNeg, Binary (ConditionXor, expr, ConditionConst const2)))
	      (fun _ -> (return []))
	| _ ->
	    match (expr, pattern) with
		(_, ExprPattern input_name) ->
		  return [ ExprBinding (input_name, expr) ]
	      | (Register register, RegisterPattern input_name) ->
		  return [ RegisterBinding (input_name, register) ]
	      | (_, RegisterPattern input_name) ->
		  return [ ExprBinding (input_name, expr) ]
	      | (LoadBO (byte_order1, width, addr),
		 LoadBOPattern (byte_order2, (widths, width_input_name), addr_pattern)) when
		  byte_order1 = byte_order2 && (mem width widths) ->
		  combine_bindings
		    (return [ WidthBinding (width_input_name, width) ])
		    (match_rec addr addr_pattern)
	      | (Unary (op1, arg), UnaryPattern (op2, arg_pattern)) when
		  op1 = op2 -> match_rec arg arg_pattern
	      | (UnaryWidth (op1, width, arg),
		 UnaryWidthPattern (op2, (widths, width_input_name), arg_pattern)) when
		  op1 = op2 && (mem width widths) ->
		  combine_bindings
		    (return [ WidthBinding (width_input_name, width) ])
		    (match_rec arg arg_pattern)
	      | (Binary (op1, arg1, arg2),
		 BinaryPattern (op2, arg_pattern1, arg_pattern2)) when
		  op1 == op2 ->
		  combine_bindings (match_rec arg1 arg_pattern1) (match_rec arg2 arg_pattern2)
	      | (BinaryWidth (op1, width, arg1, arg2),
		 BinaryWidthPattern (op2, (widths, width_input_name), arg_pattern1, arg_pattern2)) when
		  op1 = op2 && (mem width widths) ->
		  combine_bindings
		    (return [ WidthBinding (width_input_name, width) ])
		    (combine_bindings (match_rec arg1 arg_pattern1) (match_rec arg2 arg_pattern2))
	      | (TernaryWidth (op1, width, arg1, arg2, arg3),
		 TernaryWidthPattern (op2, (widths, width_input_name), arg_pattern1, arg_pattern2, arg_pattern3)) when
		  op1 = op2 && (mem width widths) ->
		  combine_bindings (return [ WidthBinding (width_input_name, width) ])
		    (combine_bindings (match_rec arg1 arg_pattern1)
		       (combine_bindings (match_rec arg2 arg_pattern2) (match_rec arg3 arg_pattern3)))
	      | (Extract (arg, start, length),
		 ExtractPattern (arg_pattern, start_pattern, length_pattern)) ->
		  combine_bindings (match_rec arg arg_pattern)
		    (combine_bindings
		       (match_expr_int_const start (cfold_expr fields start) start_pattern)
		       (match_expr_int_const length (cfold_expr fields length) length_pattern))
	      | (Insert (arg1, arg2, start, length),
		 InsertPattern (arg_pattern1, arg_pattern2, start_pattern, length_pattern)) ->
		  combine_bindings
		    (combine_bindings (match_rec arg1 arg_pattern1) (match_rec arg2 arg_pattern2))
		    (combine_bindings (match_int_const start start_pattern) (match_int_const length length_pattern))
	      | _ -> fail
  in
    match_rec expr pattern

let match_expr =
  match_expr_generic (fun _ _ _ -> cm_fail)

(*** matching against target insns ***)

let try_stmt_match fields stmt insn =
  match (stmt, insn.pattern) with
      (Store (byte_order1, width, addr, value),
       StorePattern (byte_order2, (widths, width_input_name), addr_pattern, value_pattern)) when
	byte_order1 = byte_order2 && (mem width widths) ->
	  cm_bind
            (combine_bindings
	       (cm_return [ WidthBinding (width_input_name, width) ])
	       (combine_bindings (match_expr fields addr addr_pattern) (match_expr fields value value_pattern)))
	    (fun bindings ->
	       cm_bind (insn.matcher fields bindings)
	       (fun cost ->
		  cm_return (bindings, cost)))
    | (Assign (register, expr),
       AssignPattern (input_name, pattern)) ->
	 cm_bind
	   (combine_bindings
	      (cm_return [ RegisterBinding (input_name, register) ])
	      (match_expr fields expr pattern))
	   (fun bindings ->
	      cm_bind (insn.matcher fields bindings)
	      (fun cost ->
		 cm_return (bindings, cost)))
    | _ -> cm_fail

(*
let rec find_best_expr_match matches =
  match matches with
      [] -> raise No_match
    | [m] -> m
    | m :: rest ->
	let best = find_best_expr_match rest
	in if (uncert_value m.expr_match_data.cumulative_cost) < (uncert_value best.expr_match_data.cumulative_cost) then
	    m
	  else
	    best
*)

let find_best_expr_match matches =
  find (fun m ->
	  match m.expr_match_data.cumulative_cost with
	      Certainly _ -> true
	    | Maybe _ -> false)
    matches

let calculate_submatch_data bindings best_matches_alist =
  fold_left (combine_maybe (fun cost1 cost2 -> uncert_add cost1 cost2))
    (Some (Certainly 0))
    (map (fun binding ->
            match binding with
	      ExprBinding (_, expr) ->
		(try
		  let sub_matches = assq expr best_matches_alist
		  in let match_data = (find_best_expr_match sub_matches).expr_match_data
		  in Some match_data.cumulative_cost
		with
		  Not_found -> None)
	    | _ -> Some (Certainly 0))
       bindings)

let find_matches make_match_data stmt fields insns best_matches_alist =
  let (matches, conds_list) =
    split (map (fun insn ->
                  let match_cm = try_stmt_match fields stmt insn
		  in match match_cm with
		    CMSuccess ((bindings, cost), conds) ->
		      (let maybe_sub_data = calculate_submatch_data bindings best_matches_alist
		      in match maybe_sub_data with
			Some sub_costs ->
			  ([ make_match_data insn cost (uncert_add cost sub_costs) bindings ], conds)
		      | None -> ([], conds))
		  | CMFailureWithReason (conds, fcond) ->
		      ([], (not_expr fcond) :: conds)
		  | CMFailure ->
		      ([], []))
	     insns)
  in (flatten matches, flatten conds_list)

let find_stmt_matches stmt =
  find_matches (fun insn cost cumulative_cost bindings ->
		  { matched_stmt = stmt ;
		    stmt_match_data = { target_insn = insn ;
					cost = cost ;
					cumulative_cost = cumulative_cost ;
					bindings = bindings }})
    stmt

(*
let rec next_tmp_reg_no = ref 100
and make_tmp_reg_no =
  let reg_no = !next_tmp_reg_no
  in incr next_tmp_reg_no ;
    reg_no
*)

let find_expr_matches expr =
  (* print_string "matching " ; print_expr expr ; print_string " with " ; print_matches_alist best_matches_alist ; *)
  let stmt = Assign (IntermediateRegister expr, expr)
  in find_matches (fun insn cost cumulative_cost bindings ->
		     { matched_expr = expr ;
		       expr_match_data = { target_insn = insn ;
					   cost = cost ;
					   cumulative_cost = cumulative_cost ;
					   bindings = bindings }})
       stmt

let combine_uncertain_matches cost_func matches new_match =
  let uncert_cost = cost_func new_match
  in let cost = uncert_value uncert_cost
  in if (for_all (fun m ->
		    match cost_func m with
			Maybe (cost2, _) -> true
		      | Certainly cost2 -> cost2 > cost)
	   matches) then
      match uncert_cost with
	  Maybe _ -> new_match :: matches
	| Certainly _ ->
	    new_match :: (filter (fun m -> uncert_less (cost_func m) uncert_cost) matches)
    else
      matches

let combine_uncertain_expr_matches =
  combine_uncertain_matches (fun m -> m.expr_match_data.cumulative_cost)

let combine_uncertain_stmt_matches =
  combine_uncertain_matches (fun m -> m.stmt_match_data.cumulative_cost)

let check_for_certain_matches best_matches_alist match_datas =
  let rec check match_datas =
    iter (fun match_data ->
	    iter (fun binding ->
		    match binding with
			ExprBinding (_, expr) ->
			  let matches = assoc expr best_matches_alist
			  in check (map (fun m -> m.expr_match_data) matches) ;
			    if exists (fun m -> match m.expr_match_data.cumulative_cost with
					   Maybe _ -> false
					 | Certainly _ -> true) matches then
			      ()
			    else
			      (print_string "no certain match for expr " ; print_expr expr ; print_newline () ;
			       raise No_match)
		      | _ -> ())
	      match_data.bindings)
      match_datas
  in check match_datas

let rec recursively_match_stmt fields stmt target_insns =
  let rec recursively_match_expr (best_matches_alist, conds) expr =
    let subs = if is_const (cfold_expr fields expr) then [] else expr_sub_exprs expr
    in let (best_matches_alist, conds) = fold_left recursively_match_expr (best_matches_alist, conds) subs
    in let (matches, match_conds) = find_expr_matches expr fields target_insns best_matches_alist
    in let matches = fold_left combine_uncertain_expr_matches [] matches
    in ((expr, matches) :: best_matches_alist, match_conds @ conds)
  in let subs = stmt_sub_exprs stmt
  in let (best_matches_alist, conds) = fold_left recursively_match_expr ([], []) subs
  in let (matches, match_conds) = find_stmt_matches stmt fields target_insns best_matches_alist
  in let matches = fold_left combine_uncertain_stmt_matches [] matches
  in if exists (fun m -> match m.stmt_match_data.cumulative_cost with Maybe _ -> false | Certainly _ -> true) matches then
      (check_for_certain_matches best_matches_alist (map (fun m -> m.stmt_match_data) matches) ;
       (matches, best_matches_alist, match_conds @ conds))
    else
      (print_string "no certain match for stmt " ; print_stmt stmt ;
       raise No_match)
