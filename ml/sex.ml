(*
 * sex.ml
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

open Bitmath
open Expr
open Mapping
open Monad
open Cond_monad

(* returns the number of sign extended bits, i.e., the number of upper bits
that are the same as the bit just below them.  this can be 0. *)
let rec int_sextension i =
  if (is_bit_set i 62) = (is_bit_set i 63) then
    add one (int_sextension (shift_left i 1))
  else
    zero

let expr_sextension mapping fields expr =
  let rec apply_unary op arg =
    match op with
	LoadByte | IntToFloat | FloatToInt | IntToCondition
      | IntEven | ConditionNeg | FloatSqrt | FloatNeg | FloatAbs
      | LowOneBits | LowMask | HighMask -> cm_return (int_literal_expr zero)
      | ConditionToInt -> cm_return (int_literal_expr 62L)
      | IntNeg ->
	  cm_bind (sextension arg)
	    (fun sarg ->
	       cm_return (UserOp ("Max", [int_literal_expr zero ; sub_expr sarg (int_literal_expr one)])))
      | BitNeg -> sextension arg
  and apply_unary_width op width arg =
    match op with
	IntParityEven | IntSign -> cm_return (int_literal_expr zero)
      | Sex ->
	  let num_sex_bits = int_literal_expr (of_int (64 - (width * 8)))
	  in cm_bind (sextension arg)
	       (fun sarg ->
		  cm_return (UserOp ("Max", [num_sex_bits ; sarg])))
      | Zex ->
	  if width = 8 then
	    cm_return (int_literal_expr zero)
	  else
	    cm_return (int_literal_expr (of_int (63 - (width * 8))))
  and apply_binary op arg1 arg2 =
    match op with
	IntEqual | LessU | LessS | FloatEqual | FloatLess | ConditionAnd | ConditionOr | ConditionXor
      | FloatAdd | FloatSub | FloatMul | FloatDiv
      | BothLowOneBits | BitMask ->
	  cm_return (int_literal_expr zero)
      | IntAdd ->
	  (make_bind2 cm_bind cm_bind)
	    (sextension arg1)
	    (sextension arg2)
	    (fun sarg1 sarg2 ->
	       cm_return (UserOp ("Max", [sub_expr (UserOp ("Min", [sarg1 ; sarg2])) (int_literal_expr one) ;
					  int_literal_expr zero])))
      | IntMul ->
	  (make_bind2 cm_bind cm_bind)
	    (sextension arg1)
	    (sextension arg2)
	    (fun sarg1 sarg2 ->
	       cm_return (UserOp ("Max", [sub_expr (add_expr (sarg1) (sarg2)) (int_literal_expr 63L) ;
					  int_literal_expr zero])))
      | BitAnd | BitOr | BitXor ->
	  (make_bind2 cm_bind cm_bind)
	    (sextension arg1)
	    (sextension arg2)
	    (fun sarg1 sarg2 ->
	       cm_return (UserOp ("Min", [sarg1 ; sarg2])))
      | ShiftL ->
	  if is_const (cfold_expr fields arg2) then
	    cm_bind (sextension arg1)
	      (fun sarg1 ->
		 cm_return (UserOp ("Max", [sub_expr sarg1 arg2 ; int_literal_expr zero])))
	  else
	    cm_return (int_literal_expr zero)
  and apply_binary_width op width arg1 arg2 =
    match op with
	AddCarry | SubCarry | Overflow -> cm_return (int_literal_expr zero)
      | LShiftR ->
	  if (width = 8) && (is_const (cfold_expr fields arg2)) then
	    cm_return (UserOp ("Max", [sub_expr arg2 (int_literal_expr one) ; int_literal_expr zero]))
	  else
	    cm_return (int_literal_expr zero)
      | AShiftR ->
	  if width = 8 then
	    cm_bind (sextension arg1)
	      (fun sarg1 ->
		 if is_const (cfold_expr fields arg2) then
		   cm_return (UserOp ("Min", [add_expr sarg1 arg2 ; int_literal_expr 63L]))
		 else
		   cm_return sarg1)
	  else
	    cm_return (int_literal_expr zero)
      | IntMulHS | IntMulHU ->
	  cm_return (int_literal_expr zero)
  and apply_ternary_width op width arg1 arg2 arg3 =
    cm_return (int_literal_expr zero)
  and sextension expr =
    if is_const (cfold_expr fields expr) then
      cm_return (UserOp ("IntSextension", [expr]))
    else
      match expr with
	  Register (reg) -> cm_return (int_literal_expr (mapping.register_sextension reg))
	| LoadBO _ -> cm_return (int_literal_expr zero)
	| Unary (op, arg) -> apply_unary op arg
	| UnaryWidth (op, width, arg) -> apply_unary_width op width arg
	| Binary (op, arg1, arg2) -> apply_binary op arg1 arg2
	| BinaryWidth (op, width, arg1, arg2) -> apply_binary_width op width arg1 arg2
	| TernaryWidth (op, width, arg1, arg2, arg3) -> apply_ternary_width op width arg1 arg2 arg3
	| Extract (arg, start, length) ->
	    cm_return (UserOp ("Max", [sub_expr (int_literal_expr 63L) length ; int_literal_expr zero]))
	| Insert (template, value, start, length) ->
	    let last_bit = add_expr (IntConst start) (IntConst length)
	    in if is_const (cfold_expr fields last_bit) then
		cm_if fields (Binary (IntEqual, last_bit, int_literal_expr 64L))
		  (fun _ -> sextension (shiftl_expr value (IntConst start)))
		  (fun _ ->
		     cm_bind (sextension template)
		     (fun stemplate ->
			cm_return (UserOp ("Max", [UserOp ("Min", [sub_expr (stemplate) (int_literal_expr one) ;
								   sub_expr (int_literal_expr 63L) last_bit]) ;
						   int_literal_expr zero]))))
	      else
		cm_return (int_literal_expr zero)
	| If (_, cons, alt) ->
	    (make_bind2 cm_bind cm_bind)
	      (sextension cons)
	      (sextension alt)
	      (fun scons salt ->
		 cm_return (UserOp ("Min", [scons ; salt])))
	| _ -> cm_return (int_literal_expr zero)
  in sextension expr
