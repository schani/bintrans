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

open Utils
open Expr
open Monad
open Cond_monad
open Target_alpha
open Matcher
open Pruner
open Mapping
open Explorer
open Switcher
open Cgen
open Irmacros
open Uncertainty

let make_rotl4 =
  make_rotl 4

let make_ppc_and ra rs rb =
  Assign (ra, Binary (BitAnd, Register rs, Register rb))

let make_ppc_andc ra rs rb =
  Assign (ra, Binary (BitAnd, Register rs, Unary (BitNeg, Register rb)))

let make_ppc_mask mb me =
  make_mask (Binary (IntSub, IntConst (IntLiteral 31L), me))
    (Binary (IntSub, IntConst (IntLiteral 31L), mb))

let make_ppc_rlwimi ra rs sh mb me =
  Assign (ra,
	  bitor_expr (bitand_expr (make_rotl4 rs sh) (make_ppc_mask mb me))
	    (bitand_expr (Register ra) (bitneg_expr (make_ppc_mask mb me))))

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

let sort_match_datas match_datas =
  sort (fun a b ->
	  match (a.cumulative_cost, b.cumulative_cost) with
	      (Certainly _, _) -> 1
	    | (_, Certainly _) -> -1
	    | (Maybe (c1, _), Maybe (c2, _)) -> c1 - c2)
    match_datas

let print_target_insns printers allocation best_matches_alist match_datas =
  let rec print_bindings bindings =
    iter (function (name, expr) ->
	    let reg_num_str = string_of_int (lookup_intermediate_reg allocation expr)
	    in print_string ("IF_NEED_REG(" ^ reg_num_str ^ ") {\n") ;
	      print_exprs best_matches_alist
		(sort_match_datas (map (fun m -> m.expr_match_data)
				     (assoc expr best_matches_alist)))
		("ALLOC_REG(" ^ reg_num_str ^ ");\n") ;
	      print_string "}\n")
      bindings
  and print best_matches_alist match_data alloc_reg_string =
    let expr_bindings  = filtermap (fun b -> match b with
					ExprBinding (name, expr) -> Some (name, expr)
				      | _ -> None)
			   match_data.bindings
    and printer = assoc match_data.target_insn.name printers
    in print_bindings expr_bindings ;
      print_string alloc_reg_string ;
      print_string (printer allocation match_data.bindings) ; print_newline () ;
      iter (function (_, expr) ->
	      print_string "FREE_REG(" ; print_int (lookup_intermediate_reg allocation expr) ; print_string ");\n")
	expr_bindings
  and print_exprs best_matches_alist match_datas alloc_reg_string =
    match match_datas with
	match_data :: rest ->
	  (match match_data.cumulative_cost with
	       Maybe (_, cond) ->
		 if rest <> [] then
		   (print_string "if (" ; print_string (expr_to_c [] cond) ; print_string ") {\n" ;
		    print best_matches_alist match_data alloc_reg_string ;
		    print_string "} else " ;
		    print_exprs best_matches_alist rest alloc_reg_string)
		 else
		   raise No_match
	     | Certainly _ ->
		 if rest = [] then
		   (print_string "{\n" ;
		    print best_matches_alist match_data alloc_reg_string ;
		    print_string "}\n")
		 else
		   raise No_match)
      |	[] -> raise No_match
  in let _ = print_exprs best_matches_alist (sort_match_datas match_datas) ""
  in ()

let print_c_func reg_type result_passed printers name stmt fields num_int_regs =
  let rec print_fields fields =
    match fields with
	[] -> ()
      |	[name] ->
	  print_string "word_64 field_" ; print_string name
      | name :: rest ->
	  print_string "word_64 field_" ; print_string name ; print_string ", ";
	  print_fields rest
  in
    if result_passed then
      print_string "void"
    else
      print_string reg_type ;
    print_string "\nmlgen_" ; print_string name ; print_string " (" ; print_fields (stmt_fields stmt) ; print_string ", " ;
    print_string (join_strings ", "
		    (map0_int (fun i -> reg_type ^ " guest_reg_" ^ (string_of_int i))
		       (if result_passed then 1 else 2) num_int_regs)) ;
    print_string ")\n{\n" ;
    if not result_passed then
      print_string (reg_type ^ " guest_reg_1;\n\n")
    else
      () ;
    let stmt_forms = explore_all_fields stmt mapping_ppc_to_alpha fields alpha_matchers
    in let switch = switch_cases (map (fun form -> (form.form_conditions, form)) stmt_forms)
    in print_switch
	 (fun form ->
	    let switch = switch_cases (map (fun form_match -> (form_match.match_conditions, form_match)) form.matches)
	    in print_switch
		 (fun form_match ->
		    let allocation = make_allocation form_match.best_sub_matches form_match.match_datas
		    in let interm_reg_strings = (map0_int (fun i -> "interm_reg_" ^ (string_of_int i))
						   1 (length allocation))
		    in print_string "{\n" ;
		      if allocation <> [] then
			(print_string (reg_type ^ " " ^ (join_strings ", " interm_reg_strings) ^ ";\n") ;
			 print_string ("int " ^ (join_strings ", " (map (fun r -> r ^ "_set = 0") interm_reg_strings)) ^ ";\n\n"))
		      else
			() ;
		      print_target_insns printers allocation form_match.best_sub_matches form_match.match_datas ;
		      print_string "}\n")
		 switch)
	 switch ;
      if not result_passed then
	print_string "\nreturn guest_reg_1;\n"
      else
	() ;
      print_string "}\n\n"

let print_test_func =
  print_c_func "word_64" false

let print_gen_func =
  print_c_func "reg_t" true

let make_registers () =
  let r1 = GuestRegister (1, Int)
  and r2 = GuestRegister (2, Int)
  and r3 = GuestRegister (3, Int)
  in (r1, r2, r3)

let alpha_wrap stmt =
  match stmt with
      Assign (GuestRegister (i, Int), expr) -> Assign (GuestRegister (i, Int), sex_expr 4 expr)
    | Assign _ -> stmt
    | Store _ -> stmt

let test_explore () =
  let (r1, r2, r3) = make_registers ()
  in let stmt1 = make_ppc_andc r1 r2 r3
  and stmt2 = Assign (r1, Binary (BitAnd, Unary (BitNeg, Register r2), Register r3))
  and stmt3 = Assign (r1, (make_rotl4 (Register r2) (IntConst (IntLiteral 8L))))
  and stmt4 = Assign (r1, IntConst (IntLiteral 8000000L))
  and stmt5 = Assign (r1, make_mask (IntConst (IntLiteral 8L)) (IntConst (IntLiteral 15L)))
  and stmt6 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
  and stmt7 = make_ppc_rlwnm r1 (Register r2) (Register r3) (IntConst (IntField "mb")) (IntConst (IntField "me"))
  and stmt8 = make_ppc_rlwimi r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
  in
(*
    print_gen_func alpha_gen_gens "rlwinm" (alpha_wrap stmt6) [("sh", 0L, 32L); ("mb", 0L, 32L); ("me", 0L, 32L)] 2
;
    print_test_func alpha_printers "rlwnm" (alpha_wrap stmt7) [("mb", 0L, 32L); ("me", 0L, 32L)] 3 ;
*)
    print_gen_func alpha_gen_gens "rlwimi" (alpha_wrap stmt8) [("sh", 0L, 32L); ("mb", 0L, 32L); ("me", 0L, 32L)] 2

let test_maybe () =
  let (r1, r2, r3) = make_registers ()
  in let stmt = Assign (r1, Binary (BitAnd, Register r2, IntConst (IntField "imm")))
  in print_gen_func alpha_gen_gens "and" stmt [] 2

let test_zapnot () =
  let (r1, r2, r3) = make_registers ()
  in let stmt = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
  in
    print_test_func alpha_printers "rlwinm" stmt [("sh", 0L, 1L); ("mb", 8L, 9L); ("me", 23L, 24L)] 2

let test_rotand () =
  let (r1, r2, r3) = make_registers ()
  in let stmt = Assign (r1, bitand_expr (shiftl_expr (Register r2) (IntConst (IntField "sh"))) (bitmask_expr (IntConst (IntField "mb")) (IntConst (IntField "me"))))
  in 
    print_test_func alpha_printers "rotand" stmt [("sh", 9L, 10L); ("mb", 0L, 16L); ("me", 0L, 16L)] 2

let main () =
  let (r1, r2, r3) = make_registers ()
  in let stmt1 = Assign (r1, Binary (BitAnd, Binary (ShiftL, Register r2, IntConst (IntField "sh")),
				     (make_mask (int_literal_expr 0L) (int_literal_expr 15L))))
     and stmt2 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
     and fields = [("sh", 0L); ("mb", 3L); ("me", 30L)]
  in let (stmt, conds) = cm_yield (simplify_and_prune_stmt_until_fixpoint mapping_ppc_to_alpha fields stmt2)
  in let conds = simplify_conditions conds
  in print_stmt stmt2 ; print_string "->\n" ;
    print_stmt (cfold_stmt [] stmt) ; print_string "=\n" ;
    print_stmt (cfold_stmt fields stmt) ; print_string "when\n" ;
    iter (fun x -> print_expr x ; print_newline ()) conds ;;

let test_sex () =
  let (r1, r2, r3) = make_registers ()
  in let stmt = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
     and fields = [("sh", 8L); ("mb", 0L); ("me", 23L)]
  in let (sstmt, conds) = cm_yield (simplify_and_prune_stmt_until_fixpoint mapping_ppc_to_alpha fields (alpha_wrap stmt))
  in let (ssstmt, conds) = cm_yield (sex_simplify_stmt mapping_ppc_to_alpha fields sstmt)
  in print_stmt (cfold_stmt [] ssstmt) ;;

(* main () ;; *)

test_explore () ;;

(* test_maybe () ;; *)

(* test_zapnot () ;; *)

(* test_rotand () ;; *)

(* test_sex () ;; *)

exit 0 ;;
