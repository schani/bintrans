(*
 * test.ml
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

open Expr
open Cond_monad
open Target_alpha
open Matcher
open Explorer
open Switcher
open Cgen
open Irmacros

let make_rotl4 =
  make_rotl 4

let make_ppc_and ra rs rb =
  Assign (ra, Binary (BitAnd, Register rs, Register rb))

let make_ppc_andc ra rs rb =
  Assign (ra, Binary (BitAnd, Register rs, Unary (BitNeg, Register rb)))

let make_ppc_mask mb me =
  make_mask (Binary (IntSub, IntConst (IntLiteral 31L), me))
    (Binary (IntSub, IntConst (IntLiteral 31L), mb))

let make_ppc_rlwinm ra rs sh mb me =
  Assign (ra, Binary (BitAnd, make_rotl4 rs sh, make_ppc_mask mb me))

let make_ppc_rlwnm ra rs rb mb me =
  Assign (ra, Binary (BitAnd, make_rotl4 rs (Binary (BitAnd, rb, int_literal_expr 0x1fL)),
		      make_ppc_mask mb me))

(*
let rec repeat_until_fixpoint fn data =
  let ndata = fn data
  in if ndata = data then
    data
  else
    repeat_until_fixpoint fn ndata
*)

let print_target_insns allocation best_matches_alist match_data =
  let rec print_bindings exprs_printed bindings =
    match bindings with
	[] -> exprs_printed
      | ExprBinding (name, expr) :: rest ->
	  let exprs_printed =
	    if not (mem expr exprs_printed) then
	      expr :: (print exprs_printed best_matches_alist (assoc expr best_matches_alist).expr_match_data)
	    else
	      exprs_printed
	  in print_bindings exprs_printed rest
      | _ :: _ -> raise Wrong_binding
  and print exprs_printed best_matches_alist match_data =
    let expr_bindings  = filter (fun b -> match b with
				     ExprBinding _ -> true
				   | _ -> false) match_data.bindings
    and printer = assoc match_data.target_insn.name alpha_printers
    in let expr_bindings = print_bindings exprs_printed expr_bindings
    in print_string (printer allocation match_data.bindings) ; print_newline () ;
      expr_bindings
  in let exprs_printed = print [] best_matches_alist match_data
  in ()

let test_expore () =
  let r1 = GuestRegister (1, Int)
  and r2 = GuestRegister (2, Int)
  and r3 = GuestRegister (3, Int)
  in let stmt1 = make_ppc_andc r1 r2 r3
  and stmt2 = Assign (r1, Binary (BitAnd, Unary (BitNeg, Register r2), Register r3))
  and stmt3 = Assign (r1, (make_rotl4 (Register r2) (IntConst (IntLiteral 8L))))
  and stmt4 = Assign (r1, IntConst (IntLiteral 8000000L))
  and stmt5 = Assign (r1, make_mask (IntConst (IntLiteral 8L)) (IntConst (IntLiteral 15L)))
  and stmt6 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
  and stmt7 = make_ppc_rlwnm r1 (Register r2) (Register r3) (IntConst (IntField "mb")) (IntConst (IntField "me"))
(*  and fields = [("sh", 16L); ("mb", 16L); ("me", 31L)]
  in let stmt = repeat_until_fixpoint (fun stmt -> (cm_value (prune_stmt fields (cm_value (simplify_stmt fields stmt))))) stmt6
  in print_stmt stmt ;
  let (best_stmt_match, best_sub_matches_alist) = recursively_match_stmt fields stmt alpha_insns
  in print_whole_match best_stmt_match best_sub_matches_alist ;
*)
  (* in let stmt_forms = explore_all_fields stmt6 [("sh", 0L, 32L); ("mb", 0L, 32L); ("me", 0L, 32L)] alpha_matchers *)
  in let stmt_forms = explore_all_fields stmt7 [("mb", 0L, 32L); ("me", 0L, 32L)] alpha_matchers
  in let switch = switch_cases (map (fun form -> (form.form_conditions, form)) stmt_forms)
  in print_switch (fun form ->
		     let switch = switch_cases (map (fun form_match -> (form_match.match_conditions, form_match)) form.matches)
		     in print_switch (fun form_match ->
					let allocation = make_allocation form_match.best_sub_matches form_match.match_data
					in print_string "  " ;
					  print_target_insns allocation form_match.best_sub_matches form_match.match_data ;
					  print_newline ())
			  switch)
       switch

let main () =
  let r1 = GuestRegister (1, Int)
  and r2 = GuestRegister (2, Int)
  and r3 = GuestRegister (3, Int)
  in let stmt1 = Assign (r1, Binary (BitAnd, Binary (ShiftL, Register r2, IntConst (IntField "sh")),
				     (make_mask (int_literal_expr 0L) (int_literal_expr 15L))))
     and stmt2 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
     and fields = [("sh", 0L); ("mb", 3L); ("me", 30L)]
  in let (stmt, conds) = cm_yield (simplify_and_prune_stmt_until_fixpoint fields stmt2)
  in let conds = simplify_conditions conds
  in print_stmt stmt2 ; print_string "->\n" ;
    print_stmt (cfold_stmt [] stmt) ; print_string "=\n" ;
    print_stmt (cfold_stmt fields stmt) ; print_string "when\n" ;
    iter (fun x -> print_expr x ; print_newline ()) conds ;;

(* main ();; *)

test_expore ();;

exit 0;;
