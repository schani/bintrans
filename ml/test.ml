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
open Sys

open Utils
open Expr
open Monad
open Cond_monad
open Target_alpha
open Machine
open Machine_ppc
open Matcher
open Pruner
open Mapping
open Explorer
open Switcher
open Cgen
open Irmacros
open Uncertainty

exception Not_guest_register
exception Width_not_supported

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

let print_c_func reg_type result_passed printers name stmt fields =
  let rec print_fields fields =
    match fields with
	[] -> ()
      |	[name] ->
	  print_string "word_64 field_" ; print_string name
      | name :: rest ->
	  print_string "word_64 field_" ; print_string name ; print_string ", ";
	  print_fields rest
  and sort_guest_regs regs =
    sort (fun (_, r1, _) (_, r2, _) ->
	    match (r1, r2) with
		(true, false) -> -1
	      | (false, true) -> 1
	      | _ -> 0)
      regs
  and handle_leaf_stmt stmt =
    let stmt_forms = explore_all_fields stmt mapping_ppc_to_alpha fields alpha_matchers
    in let switch = switch_cases (map (fun form -> (form.form_conditions, form)) stmt_forms)
    in print_switch
	 (fun form ->
	    let guest_regs = sort_guest_regs (collect_stmt_guest_regs form.stmt)
	    and switch = switch_cases (map (fun form_match -> (form_match.match_conditions, form_match)) form.matches)
	    in print_string (reg_type ^ " " ^
			     (join_strings ", "
				(map (fun (reg, read, write) ->
					match reg with
					    GuestRegister (int_const, _) ->
					      (register_to_c [] reg) ^ " = ALLOC_GUEST_REG_" ^
					      (if read then "R" else "") ^ (if write then "W" else "") ^
					      "(" ^ (int_const_to_c int_const) ^ ")"
					  | _ -> raise Not_guest_register)
				   guest_regs)) ^
			     ";\n") ;
	      print_switch
		(fun form_match ->
		   let allocation = make_allocation form_match.best_sub_matches form_match.match_datas
		   in let interm_reg_strings = (map0_int (fun i -> "interm_reg_" ^ (string_of_int i))
						  1 (length allocation))
		   in print_string "{ /* " ; print_stmt form.stmt ; print_string " */\n" ;
		     if allocation <> [] then
		       (print_string (reg_type ^ " " ^ (join_strings ", " interm_reg_strings) ^ ";\n") ;
			print_string ("int " ^ (join_strings ", " (map (fun r -> r ^ "_set = 0") interm_reg_strings)) ^ ";\n\n"))
		     else
		       () ;
		     print_target_insns printers allocation form_match.best_sub_matches form_match.match_datas ;
		     print_string "}\n")
		switch ;
	      iter (fun (reg, _, _) ->
		      match reg with
			  GuestRegister (int_const, _) ->
			    print_string ("FREE_GUEST_REG(" ^ (register_to_c [] reg) ^ ");\n")
			| _ -> raise Not_guest_register)
		guest_regs)
	 switch
  and handle_stmt stmt =
    match stmt with
	Store _ -> handle_leaf_stmt stmt
      | Assign _ -> handle_leaf_stmt stmt
      | Let (name, width, rhs, sub) ->
	  print_string ("reg_t let_reg_" ^ name ^ " = alloc_tmp_integer_reg();\n{\n") ;
	  handle_leaf_stmt (Assign (LetRegister (name, expr_value_type rhs, width), rhs)) ;
	  print_string "}\n{\n" ;
	  handle_stmt sub ;
	  print_string ("free_tmp_integer_reg(let_reg_" ^ name ^ ");\n}\n")
      | Seq (sub1, sub2) ->
	  handle_stmt sub1 ;
	  print_string "{\n" ;
	  handle_stmt sub2 ;
	  print_string "}\n"
  in
    if result_passed then
      print_string "void"
    else
      print_string reg_type ;
    print_string "\nmlgen_" ; print_string name ; print_string " (word_32 insn)\n{\n" ;
    (* print_fields (stmt_fields stmt) ; *)
    handle_stmt stmt ;
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
  let rs = GuestRegister (IntField "rs", Int)
  and rd = GuestRegister (IntField "rd", Int)
  and ra = GuestRegister (IntField "ra", Int)
  and rb = GuestRegister (IntField "rb", Int)
  in (rs, rd, ra, rb)

let rec alpha_wrap stmt =
  let wrap_addr width addr =
    match width with
	1 -> bitxor_expr addr (int_literal_expr 3L)
      | 2 -> bitxor_expr addr (int_literal_expr 2L)
      | 4 -> addr
      | _ -> raise Width_not_supported
  in let rec wrap_expr expr =
      match expr with
	  Unary (LoadByte, addr) ->
	    Unary (LoadByte, wrap_addr 1 (wrap_expr addr))
	| LoadBO (bo, width, addr) ->
	    LoadBO (other_byte_order bo, width, wrap_addr width (wrap_expr addr))
	| _ ->
	    apply_to_expr_subs wrap_expr expr
  in match stmt with
      Assign (GuestRegister (i, Int), expr) -> Assign (GuestRegister (i, Int), sex_expr 4 (wrap_expr expr))
    | Assign (reg, expr) -> Assign (reg, wrap_expr expr)
    | Store (bo, width, addr, rhs) -> Store (other_byte_order bo, width, wrap_addr width (wrap_expr addr), wrap_expr rhs)
    | Let (name, width, rhs, sub) -> Let (name, width, wrap_expr rhs, alpha_wrap sub)
    | Seq (sub1, sub2) -> Seq (alpha_wrap sub1, alpha_wrap sub2)

let print_gen machine_insn =
  print_gen_func alpha_gen_gens machine_insn.machine_insn_name (alpha_wrap machine_insn.insn_stmt) machine_insn.explore_fields

let test_maybe () =
  let (r1, r2, r3, r4) = make_registers ()
  in let stmt = Assign (r1, Binary (BitAnd, Register r2, IntConst (IntField "imm")))
  in print_gen_func alpha_gen_gens "and" stmt [] ;;

(*
let test_zapnot () =
  let (r1, r2, r3, r4) = make_registers ()
  in let stmt = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
  in
    print_test_func alpha_printers "rlwinm" stmt [("sh", 0L, 1L); ("mb", 8L, 9L); ("me", 23L, 24L)]

let test_rotand () =
  let (r1, r2, r3, r4) = make_registers ()
  in let stmt = Assign (r1, bitand_expr (shiftl_expr (Register r2) (IntConst (IntField "sh"))) (bitmask_expr (IntConst (IntField "mb")) (IntConst (IntField "me"))))
  in 
    print_test_func alpha_printers "rotand" stmt [("sh", 9L, 10L); ("mb", 0L, 16L); ("me", 0L, 16L)]

let main () =
  let (r1, r2, r3, r4) = make_registers ()
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

*)

let test_sex insn =
  let stmt = alpha_wrap insn.insn_stmt
  in print_stmt (cfold_stmt [] stmt) ;
    let (sstmt, conds) = cm_yield (simplify_and_prune_stmt_until_fixpoint mapping_ppc_to_alpha [] (alpha_wrap stmt))
    in print_stmt (cfold_stmt [] sstmt) ;
      let (ssstmt, conds) = cm_yield (sex_simplify_stmt mapping_ppc_to_alpha [] sstmt)
      in print_stmt (cfold_stmt [] ssstmt) ;;

(* main () ;; *)

(* test_explore () ;; *)

(* test_maybe () ;; *)

(* test_zapnot () ;; *)

(* test_rotand () ;; *)

(* test_sex (find_insn ppc_insns "andc") ;; *)

iter
  (fun insn_name -> print_gen (find_insn ppc_insns insn_name))
  (tl (Array.to_list argv)) ;;

exit 0 ;;
