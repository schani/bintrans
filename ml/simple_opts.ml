(*
 * simple_opts.ml
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

open Expr
open Cond_monad
open Pruner
open Irmacros

(*** sex optimization ***)

let rec sex_optimize_expr mapping fields expr =
  cm_bind (apply_to_expr_subs_with_monad cm_return cm_bind (sex_optimize_expr mapping fields) expr)
    (fun expr ->
       match expr with
	   UnaryWidth (Sex, width, Binary (BitAnd, arg1, arg2)) when is_const (cfold_expr fields arg2) ->
	     cm_if fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, expr_known mapping fields arg1,
							     int_literal_expr (of_int (width * 8 - 1)))))
	       (fun _ ->
		  cm_return (bitand_expr arg1 (bitor_expr arg2 (int_literal_expr (lognot (width_mask width))))))
	       (fun _ ->
		  cm_return expr)
	 | _ -> cm_return expr)

let sex_optimize_stmt mapping fields stmt =
  apply_to_stmt_subs_with_cond_monad (sex_optimize_expr mapping fields) stmt

(*** various little optimizations ***)

let rec simple_optimize_expr fields expr =
  if is_const (cfold_expr fields expr) then
    cm_return expr
  else
    cm_bind (apply_to_expr_subs_with_monad cm_return cm_bind (simple_optimize_expr fields) expr)
      (fun expr ->
	 match expr with
	     (*** extract optimization ***)
	     BinaryWidth (LShiftR, width, x, c) when is_const (cfold_expr fields c) ->
	       (* lshiftr x c -> extract x c (w-c) *)
	       cm_return (Extract (x, c, sub_expr (int_literal_expr (of_int (width * 8))) c))
	   | Binary (BitAnd, x, m) when is_const (cfold_expr fields m) ->
	       (* bitand x m when is_low_mask m -> extract x 0 (low_mask_length m) *)
	       cm_if fields (UserOp ("IsLowMask", [m]))
		 (fun _ ->
		    cm_return (Extract (x, int_literal_expr zero, UserOp ("LowMaskLength", [m]))))
		 (fun _ ->
		    cm_return expr)
	   | Binary (BitAnd, Extract (x, s, l), m)
	       when (is_const (cfold_expr fields m)) && (is_const (cfold_expr fields l)) ->
	       (* bitand (extract x s l) m when is_low_mask m -> extract x s (min l (low_mask_length m)) *)
	       cm_if fields (UserOp ("IsLowMask", [m]))
		 (fun _ ->
		    cm_if fields (Binary (LessU, UserOp ("LowMaskLength", [m]), l))
		      (fun _ ->
			 cm_return (Extract (x, s, UserOp ("LowMaskLength", [m]))))
		      (fun _ ->
			 cm_return (Extract (x, s, l))))
		 (fun _ ->
		    cm_return expr)
	   | Extract (Extract (x, s1, l1), s2, l2)
	       when (is_const (cfold_expr fields l1)) && (is_const (cfold_expr fields s2))
		 && (is_const (cfold_expr fields l2)) ->
	       cm_if fields (Binary (LessU, s2, l1))
		 (fun _ ->
		    cm_if fields (Binary (LessU, l1, add_expr s2 l2))
		      (fun _ ->
			 cm_return (Extract (x, add_expr s1 s2, sub_expr l1 s2)))
		      (fun _ ->
			 cm_return (Extract (x, add_expr s1 s2, l2))))
		 (fun _ ->
		    cm_return (int_literal_expr zero))
	   | _ -> cm_return expr)

let simple_optimize_stmt fields stmt =
  apply_to_stmt_subs_with_cond_monad (simple_optimize_expr fields) stmt
