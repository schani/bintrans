(*
 * mapping.ml
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

open Expr
open Cond_monad
open Machine

exception Not_supported_in_dummy_mapping

type mapping =
    { source_machine : machine ;
      needed_target_width : register -> int ;
      register_known : register -> int64 ;
      register_bits : register -> int64 ;
      register_sextension : register -> int64 ;
      condition_bit_mappings : ((register * int64 * int64) * register) list }

let mapping_needed_target_width register = 8

let mapping_ppc_to_alpha =
  { source_machine = machine_ppc ;
    needed_target_width = (fun _ -> 8) ;
    register_known = (fun _ -> zero) ;
    register_bits = (fun _ -> zero) ;
    register_sextension = (fun _ -> 32L) ;
    condition_bit_mappings =
      [ ((GuestRegister ("SPR", IntLiteral 1L, Int), 31L, 1L), GuestRegister ("CR", IntLiteral 0L, Int)) ;
	((GuestRegister ("SPR", IntLiteral 1L, Int), 30L, 1L), GuestRegister ("CR", IntLiteral 1L, Int)) ;
	((GuestRegister ("SPR", IntLiteral 1L, Int), 29L, 1L), GuestRegister ("CR", IntLiteral 2L, Int)) ;
	((GuestRegister ("SPR", IntLiteral 1L, Int), 28L, 1L), GuestRegister ("CR", IntLiteral 3L, Int)) ;
	((GuestRegister ("SPR", IntLiteral 2L, Int), 31L, 1L), GuestRegister ("CR", IntLiteral 4L, Int)) ;
	((GuestRegister ("SPR", IntLiteral 2L, Int), 29L, 1L), GuestRegister ("CR", IntLiteral 5L, Int)) ] }

let dummy_mapping =
  { source_machine = dummy_machine ;
    needed_target_width = (fun _ -> 8 ) ;
    register_known = (fun _ -> zero ) ;
    register_bits = (fun _ -> zero ) ;
    register_sextension = (fun _ -> 0L) ;
    condition_bit_mappings = [] }

let make_sex_mapping mapping one =
  { mapping with
      register_known = (fun _ -> shift_left minus_one 31) ;
      register_bits = (fun _ -> if one then shift_left minus_one 31 else zero) }

let map_condition_bits mapping fields stmt =
  let rec map_expr expr =
    match expr with
	Extract (Register (GuestRegister _ as reg), start_expr, length_expr) ->
	  (match (cfold_expr fields start_expr, cfold_expr fields length_expr) with
	       (IntConst (IntLiteral start), IntConst (IntLiteral length)) ->
		 (try
		    let reg = assoc (reg, start, length) (mapping.condition_bit_mappings)
		    in cm_when fields (and_expr (Binary (IntEqual, start_expr, int_literal_expr start),
						 Binary (IntEqual, length_expr, int_literal_expr length)))
			 (fun _ -> cm_return (Register (reg)))
		  with
		      Not_found -> cm_return (Extract (Register reg, IntConst (IntLiteral start), IntConst (IntLiteral length))))
	     | _ ->
		 cm_return (Extract (Register reg, IntConst (IntLiteral start), IntConst (IntLiteral length))))
      | _ ->
	  apply_to_expr_subs_with_monad cm_return cm_bind map_expr expr
  and map_stmt stmt =
    match stmt with
	Assign (GuestRegister _ as lhs,
		Insert (Register (GuestRegister _ as rhs_reg),
			rhs_val, start_expr, length_expr)) when lhs = rhs_reg ->
	  cm_bind (map_expr rhs_val)
	    (fun mrhs_val ->
	       match (cfold_expr fields start_expr, cfold_expr fields length_expr) with
		   (IntConst (IntLiteral start), IntConst (IntLiteral length)) ->
		     (try
			(* FIXME: this assumes that the rhs value is already clipped, but
			   we may not know that!  We should apply a mask.  *)
			let reg = assoc (lhs, start, length) (mapping.condition_bit_mappings)
			in cm_when fields (and_expr (Binary (IntEqual, start_expr, int_literal_expr start),
						     Binary (IntEqual, length_expr, int_literal_expr length)))
			     (fun _ -> cm_return (Assign (reg, mrhs_val)))
		      with
			  Not_found -> cm_return (Assign (lhs, Insert (Register rhs_reg, mrhs_val,
								       start_expr, length_expr))))
		 | _ ->
		     cm_return (Assign (lhs, Insert (Register rhs_reg, mrhs_val,
						     start_expr, length_expr))))
      | Assign (reg, rhs) ->
	  cm_bind (map_expr rhs)
	    (fun mrhs ->
	       cm_return (Assign (reg, mrhs)))
      | Store (bo, width, addr, rhs) ->
	  (make_bind2 cm_bind cm_bind)
	    (map_expr addr)
	    (map_expr rhs)
	    (fun maddr mrhs ->
	       cm_return (Store (bo, width, maddr, mrhs)))
      | Let (name, width, rhs, sub) ->
	  (make_bind2 cm_bind cm_bind)
	    (map_expr rhs)
	    (map_stmt sub)
	    (fun mrhs msub ->
	       cm_return (Let (name, width, mrhs, msub)))
      | Seq (sub1, sub2) ->
	  (make_bind2 cm_bind cm_bind)
	    (map_stmt sub1)
	    (map_stmt sub2)
	    (fun msub1 msub2 ->
	       cm_return (Seq (msub1, msub2)))
      | IfStmt (cond, sub1, sub2) ->
	  (make_bind3 cm_bind cm_bind cm_bind)
	    (map_expr cond)
	    (map_stmt sub1)
	    (map_stmt sub2)
	    (fun mcond msub1 msub2 ->
	       cm_return (IfStmt (mcond, msub1, msub2)))
  in map_stmt stmt
