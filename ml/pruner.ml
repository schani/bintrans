(*
 * pruner.ml
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

open Monad
open Bitmath
open Memoize
open Expr
open Machine
open Mapping
open Cond_monad
open Irmacros

let bool_to_int bool =
  if bool then one else zero

let full_mask =
  int_literal_expr minus_one
and empty_mask =
  int_literal_expr zero

let unmemoized_expr_known known_one_arg bits_one_arg mapping fields expr =
  let rec known expr =
    known_one_arg (mapping, fields, expr)
  and bits expr =
    bits_one_arg (mapping, fields, expr)
  and cfold =
    cfold_expr fields
  and known_unary op arg =
    match op with
	LoadByte -> empty_mask
      | IntToFloat | FloatToInt
      | ConditionToInt ->
	  If (is_bit_set_expr (known arg) (int_literal_expr zero), full_mask, empty_mask)
      | IntEven -> bool_to_int_expr (is_bit_set_expr (known arg) (int_literal_expr zero))
      | IntNeg -> Unary (LowOneBits, (known arg))
      | BitNeg -> known arg
      | ConditionNeg -> known arg
      | FloatSqrt | FloatNeg | FloatAbs -> empty_mask
      | HighMask | LowMask | LowOneBits -> empty_mask (* FIXME *)
  and known_unary_width op width arg =
    match op with
	IntZero | IntParityEven ->
	  bool_to_int_expr (BinaryWidth (IntEqual, width, known arg, full_mask))
      | IntSign -> bool_to_int_expr (is_bit_set_expr (known arg) (int_literal_expr (of_int (width * 8 - 1))))
      | Sex -> UnaryWidth (Sex, width, known arg)
      | Zex -> bitor_expr (UnaryWidth (Zex, width, known arg)) (int_literal_expr (shift_left minus_one (width * 8)))
  and known_binary op arg1 arg2 =
    match op with
	FloatEqual | FloatLess -> empty_mask
      | IntAdd | IntSub | IntMul -> both_low_one_bits_expr (known arg1) (known arg2)
      | BitAnd | ConditionAnd ->
	  let karg1 = known arg1
	  and karg2 = known arg2
	  and barg1 = bits arg1
	  and barg2 = bits arg2
	  in bitor_expr (bitand_expr karg1 karg2)
	    (bitor_expr (bitand_expr karg1 (bitneg_expr barg1))
	       (bitand_expr karg2 (bitneg_expr barg2)))
      | BitOr | ConditionOr ->
	  bitor_expr (bitand_expr (known arg1) (known arg2))
	    (bitor_expr (bits arg1) (bits arg2))
      | BitXor | ConditionXor -> bitand_expr (known arg1) (known arg2)
      | ShiftL ->
	  (match (cfold arg2) with
	    IntConst (IntLiteral amount) ->
	      bitor_expr (shiftl_expr (known arg1) arg2)
		(bitmask_expr (int_literal_expr zero) arg2)
	  | _ -> empty_mask)
      | FloatAdd | FloatSub | FloatMul | FloatDiv -> empty_mask
      | BitMask | BothLowOneBits -> empty_mask
  and known_binary_width op width arg1 arg2 =
    let bit_width = int_literal_expr (of_int (width * 8))
    and mask = int_literal_expr (width_mask width)
    in let karg1 = bitand_expr mask (known arg1)
       and karg2 = bitand_expr mask (known arg2)
    in let all_known = Binary (ConditionAnd, BinaryWidth (IntEqual, width, karg1, mask),
			       BinaryWidth (IntEqual, width, karg2, mask))
    in match op with
      IntEqual ->
	let both_known = bitand_expr karg1 karg2
	in bool_to_int_expr (Binary (ConditionOr, all_known,
				     Unary (ConditionNeg, BinaryWidth (IntEqual, 8, bitand_expr (bits arg1) both_known,
								       bitand_expr (bits arg2) both_known))))
    | LessU | LessS | AddCarry | SubCarry | Overflow ->
	bool_to_int_expr all_known
    | LShiftR ->
	(match (cfold arg2) with
	  IntConst (IntLiteral _) ->
	    (bitor_expr (BinaryWidth (LShiftR, 8, known arg1, bitand_expr arg2 mask))
	       (bitmask_expr (sub_expr bit_width arg2) (add_expr (sub_expr (int_literal_expr 64L) bit_width) arg2)))
	| _ -> empty_mask)
    | AShiftR ->
	(match (cfold arg2) with
	  IntConst (IntLiteral _) ->
	    (bitor_expr (bitand_expr (BinaryWidth (AShiftR, width, known arg1, arg2)) mask)
	       (bitmask_expr bit_width (sub_expr (int_literal_expr 64L) bit_width)))
	| _ -> empty_mask)
    | IntMulHS | IntMulHU -> empty_mask
  and known_ternary_width op width arg1 arg2 arg3 =
    empty_mask
  and unmemoized_known expr =
    match expr with
      IntConst (IntLiteral _) -> full_mask
    | IntConst _ -> empty_mask
    | FloatConst _ -> full_mask
    | ConditionConst _ -> full_mask
    | Register reg -> int_literal_expr (mapping.register_known reg)
    | LoadBO _ -> empty_mask
    | Unary (op, arg) -> known_unary op arg
    | UnaryWidth (op, width, arg) -> known_unary_width op width arg
    | Binary (op, arg1, arg2) -> known_binary op arg1 arg2
    | BinaryWidth (op, width, arg1, arg2) -> known_binary_width op width arg1 arg2
    | TernaryWidth (op, width, arg1, arg2, arg3) -> known_ternary_width op width arg1 arg2 arg3
    | Extract (arg, start, length) ->
	(let karg = known arg
	in match (start, length) with
	  (IntLiteral _, IntLiteral _) ->
	    (bitor_expr (Extract (karg, start, length))
	       (bitmask_expr (IntConst length) (sub_expr (int_literal_expr 64L) (IntConst length))))
	| (_, IntLiteral _) -> bitmask_expr (IntConst length) (sub_expr (int_literal_expr 64L) (IntConst length))
	| _ -> empty_mask)
    | Insert (arg1, arg2, start, length) ->
	(let karg1 = known arg1
	and karg2 = known arg2
	in match (start, length) with
	  (IntLiteral _, IntLiteral _) ->
	    Insert (karg1, karg2, start, length)
	| (IntLiteral _, _) ->
	    bitand_expr karg1 (bitmask_expr (int_literal_expr zero) (IntConst start))
	| _ -> empty_mask)
    | If (condition, cons, alt) ->
	let kcons = known cons
	and kalt = known alt
	in bitand_expr kcons kalt
    | UserOp _ -> empty_mask
  in if is_const (cfold_expr fields expr) then
    full_mask
  else
    unmemoized_known expr

let unmemoized_expr_bits bits_one_arg known_one_arg mapping fields expr =
  let rec known expr =
    known_one_arg (mapping, fields, expr)
  and bits expr =
    bits_one_arg (mapping, fields, expr)
  and cfold =
    cfold_expr fields
  and bits_unary op arg =
    match op with
      LoadByte -> empty_mask
    | IntToFloat | FloatToInt -> empty_mask
    | ConditionToInt ->
	bool_to_int_expr (Binary (ConditionAnd, is_bit_set_expr (known arg) (int_literal_expr zero),
				  is_bit_set_expr (bits arg) (int_literal_expr zero)))
    | IntEven ->
	bool_to_int_expr (Binary (ConditionAnd, is_bit_set_expr (known arg) (int_literal_expr zero),
				  Unary (IntEven, bits arg)))
    | IntNeg ->
	bitand_expr (Unary (IntNeg, bits arg)) (known expr)
    | BitNeg | ConditionNeg ->
	bitand_expr (bitneg_expr (bits arg)) (known expr)
    | FloatSqrt | FloatNeg | FloatAbs -> empty_mask
    | HighMask | LowMask | LowOneBits -> empty_mask
  and bits_unary_width op width arg =
    let mask = int_literal_expr (width_mask width)
    in match op with
      IntZero -> bool_to_int_expr (Binary (ConditionAnd, is_bit_set_expr (known expr) (int_literal_expr zero),
					   UnaryWidth (IntZero, width, bits arg)))
    | IntParityEven -> bool_to_int_expr (Binary (ConditionAnd, is_bit_set_expr (known expr) (int_literal_expr zero),
						 UnaryWidth (IntParityEven, width, bits arg)))
    | IntSign ->
	let sign_bit = IntLiteral (of_int (width * 8 - 1))
	in If (is_bit_set_expr (known arg) (IntConst sign_bit),
	       Extract (bits arg, sign_bit, IntLiteral 1L),
	       empty_mask)
    | Sex -> UnaryWidth (Sex, width, bits arg)
    | Zex -> UnaryWidth (Zex, width, bits arg)
  and bits_binary op arg1 arg2 =
    match op with
      FloatEqual | FloatLess -> empty_mask
    | IntAdd ->
	bitand_expr (add_expr (bits arg1) (bits arg2)) (both_low_one_bits_expr (known arg1) (known arg2))
    | IntSub ->
	bitand_expr (sub_expr (bits arg1) (bits arg2)) (both_low_one_bits_expr (known arg1) (known arg2))
    | IntMul ->
	bitand_expr (mul_expr (bits arg1) (bits arg2)) (both_low_one_bits_expr (known arg1) (known arg2))
    | BitAnd | ConditionAnd -> bitand_expr (bits arg1) (bits arg2)
    | BitOr | ConditionOr -> bitor_expr (bits arg1) (bits arg2)
    | BitXor | ConditionXor -> bitxor_expr (bits arg1) (bits arg2)
    | ShiftL ->
	(match (cfold arg2) with
	  IntConst (IntLiteral _) -> shiftl_expr (bits arg1) arg2
	| _ -> empty_mask)
    | FloatAdd | FloatSub | FloatMul | FloatDiv -> empty_mask
    | BitMask | BothLowOneBits -> empty_mask
  and bits_binary_width op width arg1 arg2 =
    let mask = int_literal_expr (width_mask width)
    and bit_width = int_literal_expr (of_int (width * 8))
    in let karg1 = bitand_expr mask (known arg1)
    and karg2 = bitand_expr mask (known arg2)
    and barg1 = bitand_expr mask (bits arg1)
    and barg2 = bitand_expr mask (bits arg2)
    in let all_known = Binary (ConditionAnd, BinaryWidth (IntEqual, width, karg1, mask),
			       BinaryWidth (IntEqual, width, karg2, mask))
    in match op with
      IntEqual | LessU | LessS ->
	bool_to_int_expr (Binary (ConditionAnd, all_known, BinaryWidth (op, width, barg1, barg2)))
    | AddCarry ->
	if width > 4 then raise Unsupported_width ;
	bool_to_int_expr (Binary (ConditionAnd, all_known, is_bit_set_expr (add_expr barg1 barg2) bit_width))
    | SubCarry ->
	if width > 4 then raise Unsupported_width ;
	bool_to_int_expr (Binary (ConditionAnd, all_known, is_bit_set_expr (sub_expr barg1 barg2) bit_width))
    | Overflow ->
	If (all_known,
	    (let overflow_bit = int_literal_expr (of_int ((width * 8) - 1))
	    in If (BinaryWidth (IntEqual, 8, extract_bit_expr barg1 overflow_bit, extract_bit_expr barg2 overflow_bit),
		   bool_to_int_expr (Binary (ConditionXor, is_bit_set_expr (add_expr barg1 barg2) overflow_bit,
					     is_bit_set_expr barg1 overflow_bit)),
		   empty_mask)),
	    empty_mask)
    | LShiftR | AShiftR->
	(match (cfold arg2) with
	  IntConst (IntLiteral _) -> BinaryWidth (op, width, barg1, arg2)
	| _ -> empty_mask)
    | IntMulHS | IntMulHU -> empty_mask
  and bits_ternary_width op width arg1 arg2 arg3 =
    empty_mask
  and unmemoized_bits expr =
    match expr with
      IntConst (IntLiteral _) -> expr
    | IntConst _ -> empty_mask
    | FloatConst float -> int_literal_expr (bits_of_float float)
    | ConditionConst bool -> int_literal_expr (bool_to_int bool)
    | Register reg -> int_literal_expr (mapping.register_bits reg)
    | LoadBO _ -> empty_mask
    | Unary (op, arg) -> bits_unary op arg
    | UnaryWidth (op, width, arg) -> bits_unary_width op width arg
    | Binary (op, arg1, arg2) -> bits_binary op arg1 arg2
    | BinaryWidth (op, width, arg1, arg2) -> bits_binary_width op width arg1 arg2
    | TernaryWidth (op, width, arg1, arg2, arg3) -> bits_ternary_width op width arg1 arg2 arg3
    | Extract (arg, start, length) ->
	(match (start, length) with
	  (IntLiteral _, IntLiteral _) ->
	    Extract (bits arg, start, length)
	| (_, IntLiteral length) -> empty_mask
	| _ -> empty_mask)
    | Insert (arg1, arg2, start, length) ->
	(match (start, length) with
	  (IntLiteral _, IntLiteral _) ->
	    Insert (bits arg1, bits arg2, start, length)
	| (IntLiteral _, _) ->
	    bitand_expr (bits arg1) (bitmask_expr (int_literal_expr zero) (IntConst start))
	| _ -> empty_mask)
    | If (condition, cons, alt) ->
	bitand_expr (bits cons) (bits alt)
    | UserOp _ -> empty_mask
  in if is_const (cfold_expr fields expr) then
      match expr_value_type expr with
	  Int -> expr
	| Float -> empty_mask
	| Condition -> bool_to_int_expr expr
    else
      unmemoized_bits expr

let (expr_known_one_arg, expr_bits_one_arg) =
  memoize2
    (fun known bits (mapping, fields, expr) ->
       unmemoized_expr_known known bits mapping fields expr)
    (fun known bits (mapping, fields, expr) ->
       unmemoized_expr_bits bits known mapping fields expr)
    
let expr_known mapping fields expr =
  expr_known_one_arg (mapping, fields, expr)
and expr_bits mapping fields expr =
  expr_bits_one_arg (mapping, fields, expr)

let rec cheap_prune fields expr =
  match expr with
    | If (condition, cons, alt) ->
	if is_const (cfold_expr fields condition) then
	  cm_if fields condition
	    (fun _ -> cheap_prune fields cons)
	    (fun _ -> cheap_prune fields alt)
	else
	  apply_to_expr_subs_with_monad cm_return cm_bind (cheap_prune fields) expr
    | _ ->
	apply_to_expr_subs_with_monad cm_return cm_bind (cheap_prune fields) expr

let unmemoized_prune_expr prune_one_arg mapping fields expr needed =
  let prune expr needed =
    prune_one_arg (mapping, fields, expr, needed)
  and return = cm_return
  and let1 = cm_bind
  and let2 = (make_bind2 cm_bind cm_bind)
  and let3 = (make_bind3 cm_bind cm_bind cm_bind)
  and if_prune = cm_if fields
  and bits expr =
    expr_bits mapping fields expr
  and known expr =
    expr_known mapping fields expr
  in let rec unmemoized_prune expr needed =
    let prune_unary op arg =
      match op with
	LoadByte | IntToFloat | FloatToInt | FloatSqrt | FloatNeg | FloatAbs ->
	  let1 (prune arg full_mask)
	    (fun parg ->
	      return (Unary (op, parg)))
      | ConditionToInt ->
	  if_prune (Unary (IntEven, needed))
	    (fun _ -> (return (IntConst (IntLiteral zero))))
	    (fun _ -> (if_prune (Unary (ConditionNeg, Unary (IntEven, known arg)))
			 (fun _ -> (return (bitand_expr (bits arg) (int_literal_expr one))))
			 (fun _ -> (let1 (prune arg (int_literal_expr one))
				      (fun parg ->
					 return (Unary (ConditionToInt, parg)))))))
      | IntEven ->
	  if_prune (Unary (IntEven, known arg))
	    (fun _ -> (let1 (prune arg (int_literal_expr one))
			 (fun parg ->
			    return (Unary (IntEven, parg)))))
	    (fun _ -> (return (Unary (IntEven, bits arg))))
      | IntNeg ->
	  let1 (prune arg (low_mask_expr needed))
	    (fun parg ->
	      return (Unary (IntNeg, parg)))
      | BitNeg | ConditionNeg ->
	  let1 (prune arg needed)
	    (fun parg ->
	      return (Unary (op, parg)))
      | HighMask | LowMask | LowOneBits ->
	  let1 (prune arg full_mask)
	    (fun parg ->
	       return (Unary (op, parg)))
    and prune_unary_width op width arg =
      match op with
	  IntZero | IntParityEven | Zex ->
	    let1 (prune arg (int_literal_expr (width_mask width)))
	    (fun parg ->
	       return (UnaryWidth (op, width, parg)))
	| Sex ->
	    let karg = known arg
	    and barg = bits arg
	    and width_mask_expr = int_literal_expr (width_mask width)
	    and upper_mask = int_literal_expr (shift_right (lognot (width_mask width)) 1)
	    in if_prune (or_expr (bitsubset_expr needed width_mask_expr)
			   (and_expr (bitsubset_expr upper_mask karg)
			      (make_zero_or_full_p 8
				 (BinaryWidth (AShiftR, 8, barg, int_literal_expr (sub (mul (of_int width) 8L) 1L))))))
		 (fun _ -> prune arg needed)
		 (fun _ ->
		    (let1 (prune arg width_mask_expr)
		       (fun parg ->
			  return (UnaryWidth (op, width, parg)))))
	| IntSign -> 
	    let1 (prune arg (int_literal_expr (bitmask (of_int (width * 8 - 1)) 1L)))
	      (fun parg ->
		 return (UnaryWidth (op, width, parg)))
    and prune_binary op arg1 arg2 =
      match op with
	FloatEqual | FloatLess | FloatAdd | FloatSub | FloatMul | FloatDiv ->
	  let2 (prune arg1 full_mask) (prune arg2 full_mask)
	    (fun parg1 parg2 ->
	      return (Binary (op, parg1, parg2)))
      | IntAdd | IntSub | IntMul ->
	  let2 (prune arg1 (low_mask_expr needed)) (prune arg2 (low_mask_expr needed))
	    (fun parg1 parg2 ->
	      return (Binary (op, parg1, parg2)))
      | BitAnd | ConditionAnd ->
	  let karg1 = known arg1
	  and karg2 = known arg2
	  and barg1 = bits arg1
	  and barg2 = bits arg2
	  in if_prune (Binary (ConditionAnd, (bitsubset_expr needed karg1),
			       (bitsubset_expr (bitand_expr (bitneg_expr barg1) needed)
				  (bitand_expr karg2 (bitneg_expr barg2)))))
	       (fun _ -> (prune arg2 needed))
	       (fun _ -> (if_prune (Binary (ConditionAnd, (bitsubset_expr needed karg2),
					    (bitsubset_expr (bitand_expr (bitneg_expr barg2) needed)
					       (bitand_expr karg1 (bitneg_expr barg1)))))
			    (fun _ -> prune arg1 needed)
			    (fun _ -> (let2
					 (prune arg1 (bitand_expr needed (bitneg_expr (bitand_expr karg2 (bitneg_expr barg2)))))
					 (prune arg2 (bitand_expr needed (bitneg_expr (bitand_expr karg1 (bitneg_expr barg1)))))
					 (fun parg1 parg2 ->
					    (return (Binary (op, parg1, parg2))))))))
      | BitOr | ConditionOr ->
	  let karg1 = known arg1
	  and karg2 = known arg2
	  and barg1 = bits arg1
	  and barg2 = bits arg2
	  in if_prune (Binary (ConditionAnd, bitsubset_expr needed karg1,
			       UnaryWidth (IntZero, 8, bitand_expr barg1 needed)))
	       (fun _ -> (prune arg2 needed))
	       (fun _ -> (if_prune (Binary (ConditionAnd, bitsubset_expr needed karg2,
					    UnaryWidth (IntZero, 8, bitand_expr barg2 needed)))
			    (fun _ -> (prune arg1 needed))
			    (fun _ -> (let2
					 (prune arg1 (bitand_expr needed (bitneg_expr (bitand_expr karg2 barg2))))
					 (prune arg2 (bitand_expr needed (bitneg_expr (bitand_expr karg1 barg1))))
					 (fun parg1 parg2 ->
					    (return (Binary (op, parg1, parg2))))))))
      | BitXor | ConditionXor->
	  let karg1 = known arg1
	  and karg2 = known arg2
	  and barg1 = bits arg1
	  and barg2 = bits arg2
	  in if_prune (Binary (ConditionAnd, bitsubset_expr needed karg1,
			       UnaryWidth (IntZero, 8, bitand_expr barg1 needed)))
	       (fun _ -> (prune arg2 needed))
	       (fun _ -> (if_prune (Binary (ConditionAnd, bitsubset_expr needed karg2,
					    UnaryWidth (IntZero, 8, bitand_expr barg2 needed)))
			    (fun _ -> (prune arg1 needed))
			    (fun _ -> (let2
					 (prune arg1 needed)
					 (prune arg2 needed)
					 (fun parg1 parg2 ->
					    (return (Binary (op, parg1, parg2))))))))
      | ShiftL ->
	  let karg2 = known arg2
	  and barg2 = bits arg2
	  and mask = (int_literal_expr 0x3fL)
	  in if_prune (bitsubset_expr mask karg2)
	       (fun _ -> (if_prune (UnaryWidth (IntZero, 8, (bitand_expr barg2 mask)))
			    (fun _ -> (prune arg1 needed))
			    (fun _ -> (let2
					 (prune arg1 (BinaryWidth (LShiftR, 8, needed, bitand_expr barg2 mask)))
					 (prune arg2 full_mask)
					 (fun parg1 parg2 ->
					    (return (Binary (ShiftL, parg1, parg2))))))))
	       (fun _ -> (let2
			    (prune arg1 (low_mask_expr needed))
			    (prune arg2 full_mask)
			    (fun parg1 parg2 ->
			       (return (Binary (ShiftL, parg1, parg2))))))
      | BitMask | BothLowOneBits ->
	  let2
	    (prune arg1 full_mask)
	    (prune arg2 full_mask)
	    (fun parg1 parg2 ->
	       return (Binary (op, parg1, parg2)))
    and prune_binary_width op width arg1 arg2 =
      let mask = int_literal_expr (width_mask width)
      and shift_mask = int_literal_expr (width_shift_mask width)
      in match op with
	IntEqual | LessU | LessS | AddCarry | SubCarry | Overflow ->
	  if_prune (Unary (IntEven, known expr))
	    (fun _ -> (let2
			 (prune arg1 mask)
			 (prune arg2 mask)
			 (fun parg1 parg2 ->
			    (return (BinaryWidth (op, width, parg1, parg2))))))
	    (fun _ -> (return (Unary (ConditionNeg, (Unary (IntEven, bits expr))))))
      | LShiftR ->
	  let barg2 = bits arg2
	  in if_prune (bitsubset_expr shift_mask (known arg2))
	       (fun _ -> (if_prune (UnaryWidth (IntZero, 8, (bitand_expr barg2 shift_mask)))
			    (fun _ -> (prune arg1 (bitand_expr needed mask)))
			    (fun _ -> (let2
					 (prune arg1 (bitand_expr (shiftl_expr needed (bitand_expr barg2 shift_mask)) mask))
					 (prune arg2 full_mask)
					 (fun parg1 parg2 ->
					    (return (BinaryWidth (LShiftR, width, parg1, parg2))))))))
	       (fun _ -> (let2
			    (prune arg1 (bitand_expr (high_mask_expr needed) mask))
			    (prune arg2 full_mask)
			    (fun parg1 parg2 ->
			       (return (BinaryWidth (LShiftR, width, parg1, parg2))))))
      | AShiftR ->
	  let barg2 = bits arg2
	  in if_prune (bitsubset_expr shift_mask (known arg2))
	       (fun _ ->
		  (if_prune (UnaryWidth (IntZero, 8, (bitand_expr barg2 shift_mask)))
		     (fun _ -> (prune arg1 (bitand_expr needed mask)))
		     (* else *)
		     (fun _ ->
			(let amount = bitand_expr barg2 shift_mask
			 in let high_bit_mask = bitand_expr (bitneg_expr (BinaryWidth (LShiftR, 8, mask, amount))) mask
			 in let need_high_bit = Unary (ConditionNeg, (UnaryWidth (IntZero, 8, (bitand_expr needed high_bit_mask))))
			 in let1
			      (if_prune need_high_bit
				 (fun _ -> (return (bitmask_expr (int_literal_expr (of_int (width * 8 - 1))) (int_literal_expr one))))
				 (fun _ -> (return (int_literal_expr zero))))
			      (fun high_bit ->
				 let2
				 (prune arg1 (bitor_expr high_bit
						(bitand_expr (shiftl_expr needed (bitand_expr barg2 shift_mask))
						   mask)))
				 (prune arg2 shift_mask)
				 (fun parg1 parg2 ->
				    (return (BinaryWidth (AShiftR, width, parg1, parg2)))))))))
	       (* else *)
	       (fun _ -> (let2
			    (prune arg1 (bitand_expr (high_mask_expr needed) mask))
			    (prune arg2 shift_mask)
			    (fun parg1 parg2 ->
			       return (BinaryWidth (AShiftR, width, parg1, parg2)))))
      | IntMulHS | IntMulHU ->
	  let2
	    (prune arg1 mask)
	    (prune arg2 mask)
	    (fun parg1 parg2 ->
	      return (BinaryWidth (op, width, parg1, parg2)))
    and prune_ternary_width op width arg1 arg2 arg3 =
      let3
	(prune arg1 full_mask)
	(prune arg2 full_mask)
	(prune arg3 full_mask)
	(fun parg1 parg2 parg3 ->
	  return (TernaryWidth (op, width, parg1, parg2, parg3)))
    in
    if_prune (BinaryWidth (IntEqual, 8, bitand_expr (known expr) needed, needed))
      (fun _ -> (if is_const (cfold_expr fields expr) then
		   cheap_prune fields expr
		 else
		   let bits = bits expr
		   in match expr_value_type expr with
		       Int -> return bits
		     | Float -> raise Wrong_type
		     | Condition -> return (is_bit_set_expr bits (int_literal_expr zero))))
      (* else *)
      (fun _ ->
	 (match expr with
	      IntConst (IntLiteral _) -> return (bitand_expr expr needed)
	    | IntConst _ | FloatConst _ | ConditionConst _ | Register _ -> return expr
	    | LoadBO (bo, width, addr) ->
		let1
		  (prune addr (int_literal_expr (width_mask (mapping.source_machine.addr_width))))
		  (fun paddr ->
		     return (LoadBO (bo, width, paddr)))
	    | Unary (op, arg) -> prune_unary op arg
	    | UnaryWidth (op, width, arg) -> prune_unary_width op width arg
	    | Binary (op, arg1, arg2) -> prune_binary op arg1 arg2
	    | BinaryWidth (op, width, arg1, arg2) -> prune_binary_width op width arg1 arg2
	    | TernaryWidth (op, width, arg1, arg2, arg3) -> prune_ternary_width op width arg1 arg2 arg3
	    | Extract (arg, start, length) ->
		(match (start, length) with
		     (IntLiteral _, IntLiteral _) ->
		       let upper_mask = shiftl_expr (int_literal_expr minus_one) (add_expr (IntConst start) (IntConst length))
		       in let1
			    (prune arg (bitand_expr (shiftl_expr needed (IntConst start))
					  (bitmask_expr (IntConst start) (IntConst length))))
			    (fun parg ->
			       if_prune (Binary (ConditionAnd, UnaryWidth (IntZero, 8, bitxor_expr upper_mask (known arg)),
						 UnaryWidth (IntZero, 8, bitand_expr upper_mask (bits arg))))
			         (fun _ -> (return (BinaryWidth (LShiftR, 8, parg, IntConst start))))
			         (fun _ -> (return (Extract (parg, start, length)))))
		   | (IntLiteral _, _) ->
		       let1
			 (prune arg (shiftl_expr needed (IntConst start)))
			 (fun parg ->
			    return (Extract (parg, start, length)))
		   | _ ->
		       let1
			 (prune arg full_mask)
			 (fun parg ->
			    return (Extract (parg, start, length))))
	    | Insert (arg1, arg2, start, length) ->
		(match (start, length) with
		     (IntLiteral _, IntLiteral _) ->
		       if_prune (UnaryWidth (IntZero, 8, (Extract (needed, start, length))))
		         (fun _ -> (prune arg1 (Insert (needed, int_literal_expr zero, start, length))))
		         (fun _ ->
			    (let1
			       (prune arg2 (Extract (needed, start, length)))
			       (fun parg2 ->
				  (if_prune (UnaryWidth (IntZero, 8,
							 (bitand_expr needed
							    (bitneg_expr (bitmask_expr (IntConst start) (IntConst length))))))
				     (fun _ -> (return (Binary (ShiftL, parg2, IntConst start))))
				     (fun _ -> (let1
						  (prune arg1 (Insert (needed, int_literal_expr zero, start, length)))
						  (fun parg1 ->
						     (return (Insert (parg1, parg2, start, length))))))))))
		   | (IntLiteral _, _) ->
		       let1
			 (prune arg1 needed)
			 (fun parg1 ->
			    if_prune (UnaryWidth (IntZero, 8, (BinaryWidth (LShiftR, 8, needed, (IntConst start)))))
			      (fun _ -> (return parg1))
			      (fun _ -> (let1
					   (prune arg2 (BinaryWidth (LShiftR, 8, needed, (IntConst start))))
					   (fun parg2 ->
					      (return (Insert (parg1, parg2, start, length)))))))
		   | _ ->
		       let2
			 (prune arg1 needed)
			 (prune arg2 (int_literal_expr minus_one))
			 (fun parg1 parg2 ->
			    return (Insert (parg1, parg2, start, length))))
	    | If (condition, cons, alt) ->
		let3
		  (prune condition (int_literal_expr one))
		  (prune cons needed)
		  (prune alt needed)
		  (fun pcondition pcons palt ->
		     if is_const (cfold_expr fields pcondition) then
		       if_prune pcondition
			 (fun _ -> return pcons)
			 (fun _ -> return palt)
		     else
		       return (If (pcondition, pcons, palt)))
	    | UserOp (name, args) ->
		match args with
		    [] -> return (UserOp (name, []))
		  | [a] ->
		      let1 (prune a full_mask)
			(fun pa -> return (UserOp (name, [pa])))
		  | [a ; b] ->
		      let2 (prune a full_mask) (prune b full_mask)
			(fun pa pb -> return (UserOp (name, [pa ; pb])))
		  | [a ; b ; c] ->
		      let3 (prune a full_mask) (prune b full_mask) (prune c full_mask)
			(fun pa pb pc -> return (UserOp (name, [pa ; pb ; pc])))
		  | _ -> raise User_op_args_unhandled))
  in unmemoized_prune expr needed

let rec prune_expr_one_arg =
  memoize (fun prune (mapping, fields, expr, needed) ->
	     unmemoized_prune_expr prune mapping fields expr needed)
and prune_expr mapping fields expr needed =
  prune_expr_one_arg (mapping, fields, expr, (int_literal_expr needed))

let rec prune_stmt mapping fields stmt =
  match stmt with
    Store (byte_order, width, addr, value) ->
      (make_bind2 cm_bind cm_bind)
	(prune_expr mapping fields addr (width_mask (mapping.source_machine.addr_width)))
	(prune_expr mapping fields value (width_mask width))
	(fun paddr pvalue ->
	  cm_return (Store (byte_order, width, paddr, pvalue)))
  | Assign (register, value) ->
      cm_bind
	(prune_expr mapping fields value (width_mask (mapping.needed_target_width register)))
	(fun pvalue ->
	  cm_return (Assign (register, pvalue)))
  | Let (name, width, rhs, sub) ->
      (make_bind2 cm_bind cm_bind)
	(prune_expr mapping fields rhs (width_mask width))
	(prune_stmt mapping fields sub)
	(fun prhs psub ->
	   cm_return (Let (name, width, prhs, psub)))
  | Seq (sub1, sub2) ->
      (make_bind2 cm_bind cm_bind)
	(prune_stmt mapping fields sub1)
	(prune_stmt mapping fields sub2)
	(fun psub1 psub2 ->
	   cm_return (Seq (psub1, psub2)))

(*** sex optimization ***)

let rec optimize_sex_expr mapping fields expr =
  cm_bind (apply_to_expr_subs_with_monad cm_return cm_bind (optimize_sex_expr mapping fields) expr)
    (fun expr ->
       match expr with
	   UnaryWidth (Sex, width, Binary (BitAnd, arg1, arg2)) when is_const (cfold_expr fields arg2) ->
	     cm_if fields (make_zero_or_full_p 8 (BinaryWidth (AShiftR, 8, expr_known mapping fields arg1,
							       int_literal_expr (of_int (width * 8 - 1)))))
	       (fun _ ->
		  cm_return (bitand_expr arg1 (bitor_expr arg2 (int_literal_expr (lognot (width_mask width))))))
	       (fun _ ->
		  cm_return expr)
	 | _ -> cm_return expr)

let optimize_sex_stmt mapping fields stmt =
  apply_to_stmt_subs_with_cond_monad (optimize_sex_expr mapping fields) stmt
