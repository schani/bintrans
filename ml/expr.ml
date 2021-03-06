(*
 * expr.ml
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

open Utils
open Bitmath
open Monad

exception Wrong_type
exception Illegal_arguments
exception Unsupported_width
exception Unsupported_operation
exception Expression_not_const
exception Unknown_user_op
exception Wrong_user_op_args
exception User_op_called_incorrectly
exception User_op_args_unhandled
exception Stmt_expected
exception Expr_expected

type byte_order =
    BigEndian
  | LittleEndian

type value_type =
    Int
  | Float
  | Condition

type width = int

type precision =
    Single
  | Double

let width_mask width =
  match width with
      1 -> 0xffL
    | 2 -> 0xffffL
    | 4 -> 0xffffffffL
    | 8 -> 0xffffffffffffffffL
    | _ -> raise Unsupported_width

let width_shift_mask width =
  match width with
      1 -> 0x7L
    | 2 -> 0xfL
    | 4 -> 0x1fL
    | 8 -> 0x3fL
    | _ -> raise Unsupported_width

(*** byte order ***)

let other_byte_order bo =
  match bo with
      LittleEndian -> BigEndian
    | BigEndian -> LittleEndian

(*** ops ***)

type unary_op =
    LoadByte
  | IntToFloat | FloatToInt | ConditionToInt | IntToCondition
  | IntEven
  | IntNeg | BitNeg | ConditionNeg
  | FloatSqrt | FloatNeg | FloatAbs
  | LowOneBits | LowMask | HighMask	(* must only be used in constants! *)

let unary_op_value_type op =
  match op with
    LoadByte -> Int
  | IntToFloat -> Float | FloatToInt -> Int | ConditionToInt -> Int | IntToCondition -> Condition
  | IntEven -> Condition
  | IntNeg -> Int | BitNeg -> Int | ConditionNeg -> Condition
  | FloatSqrt -> Float | FloatNeg -> Float | FloatAbs -> Float
  | HighMask -> Int | LowMask -> Int | LowOneBits -> Int

let unary_op_name unary_op =
  match unary_op with
    LoadByte -> "MEM1"
  | IntToFloat -> "Float" | FloatToInt -> "Int" | ConditionToInt -> "Int" | IntToCondition -> "Condition"
  | IntEven -> "IntEven"
  | IntNeg -> "IntNeg" | BitNeg -> "BitNeg" | ConditionNeg -> "ConditionNeg"
  | FloatSqrt -> "FloatSqrt" | FloatNeg -> "FloatNeg" | FloatAbs -> "FloatAbs"
  | HighMask -> "HighMask" | LowMask -> "LowMask" | LowOneBits -> "LowOneBits"

type unary_width_op =
    IntParityEven | IntSign
  | Sex | Zex

let unary_width_op_value_type op =
  match op with
    IntParityEven -> Condition | IntSign -> Condition
  | Sex -> Int | Zex -> Int

let unary_width_op_name unary_width_op =
  match unary_width_op with
    IntParityEven -> "IntParityEven" | IntSign -> "IntSign"
  | Sex -> "Sex" | Zex -> "Zex"

type binary_op =
    IntEqual | LessU | LessS
  | FloatEqual | FloatLess
  | IntAdd | IntMul | BitAnd | BitOr | BitXor
  | ShiftL
  | ConditionAnd | ConditionOr | ConditionXor
  | FloatAdd | FloatSub | FloatMul | FloatDiv
  | BothLowOneBits | BitMask		(* must only be used in constants! *)

let binary_op_value_type op =
  match op with
    IntEqual -> Condition | LessU -> Condition | LessS -> Condition
  | FloatEqual -> Condition | FloatLess -> Condition
  | IntAdd -> Int | IntMul -> Int
  | BitAnd -> Int | BitOr -> Int | BitXor -> Int
  | ShiftL -> Int
  | ConditionAnd -> Condition | ConditionOr -> Condition | ConditionXor -> Condition
  | FloatAdd -> Float | FloatSub -> Float | FloatMul -> Float | FloatDiv -> Float
  | BitMask -> Int | BothLowOneBits -> Int

let binary_op_name binary_op =
  match binary_op with
    IntEqual -> "IntEqual" | LessU -> "IntLessU" | LessS -> "IntLessS"
  | FloatEqual -> "FloatEqual" | FloatLess -> "FloatLess"
  | IntAdd -> "IntAdd" | IntMul -> "IntMul"
  | BitAnd -> "BitAnd" | BitOr -> "BitOr" | BitXor -> "BitXor"
  | ShiftL -> "ShiftL"
  | ConditionAnd -> "ConditionAnd" | ConditionOr -> "ConditionOr" | ConditionXor -> "ConditionXor"
  | FloatAdd -> "FloatAdd" | FloatSub -> "FloatSub" | FloatMul -> "FloatMul" | FloatDiv -> "FloatDiv"
  | BitMask -> "BitMask" | BothLowOneBits -> "BothLowOneBits"

type binary_width_op =
    AddCarry | SubCarry | Overflow
  | LShiftR | AShiftR
  | IntMulHS | IntMulHU

let binary_width_op_value_type op =
  match op with
    AddCarry -> Condition | SubCarry -> Condition | Overflow -> Condition
  | LShiftR -> Int | AShiftR -> Int
  | IntMulHS -> Int | IntMulHU -> Int

let binary_width_op_name binary_width_op =
  match binary_width_op with
    AddCarry -> "AddCarry" | SubCarry -> "SubCarry" | Overflow -> "Overflow"
  | LShiftR -> "LShiftR" | AShiftR -> "AShiftR"
  | IntMulHS -> "IntMulHS" | IntMulHU -> "IntMulHU"

type ternary_width_op =
    DivS | DivU | ModS | ModU

let ternary_width_op_value_type _ = Int

let ternary_width_op_name ternary_width_op =
  match ternary_width_op with
    DivS -> "DivS" | DivU -> "DivU" | ModS -> "ModS" | ModU -> "ModU"

(*** expressions ***)

(* types *)

type input_name = string

type int_const =
    IntLiteral of int64
  | IntField of input_name

type register =
    GuestRegister of string * int_const * value_type (* class name, index, value type *)
  | HostRegister of int * value_type
  | IntermediateRegister of expr
  | LetRegister of input_name * value_type * width
and expr =
    IntConst of int_const
  | FloatConst of float
  | ConditionConst of bool
  | Register of register
  | LoadBO of byte_order * width * expr
  | Unary of unary_op * expr
  | UnaryWidth of unary_width_op * width * expr
  | Binary of binary_op * expr * expr
  | BinaryWidth of binary_width_op * width * expr * expr
  | TernaryWidth of ternary_width_op * width * expr * expr * expr
  | Extract of expr * expr * expr	(* the last two exprs must be constant *)
  | Insert of expr * expr * expr * expr (* the last two exprs must be constant *)
  | If of expr * expr * expr
  | UserOp of string * (expr list)

type stmt =
    Store of byte_order * width * expr * expr
  | Assign of register * expr
  | Let of input_name * width * expr * stmt
  | Seq of stmt * stmt
  | IfStmt of expr * stmt * stmt

type stmt_or_expr =
    Stmt of stmt
  | Expr of expr

let get_stmt soe =
  match soe with
      Stmt stmt -> stmt
    | Expr _ -> raise Stmt_expected

let get_expr soe =
  match soe with
      Stmt _ -> raise Stmt_expected
    | Expr expr -> expr

(* user ops *)

type user_op =
    { op_name : string ;
      num_args : int ;
      result_type : value_type ;
      apply_op : int64 list -> expr }

let user_ops = [
  { op_name = "IsMaskMask" ;
    num_args = 2 ;
    result_type = Condition ;
    apply_op = (function [m ; w] -> ConditionConst (is_mask_mask m (to_int w)) | _ -> raise User_op_called_incorrectly) } ;
  { op_name = "IsLowMask" ;
    num_args = 1 ;
    result_type = Condition ;
    apply_op = (function [m] -> ConditionConst (is_low_mask m) | _ -> raise User_op_called_incorrectly) } ;
  { op_name = "LowMaskLength" ;
    num_args = 1 ;
    result_type = Int ;
    apply_op = (function [m] -> IntConst (IntLiteral (low_mask_length m)) | _ -> raise User_op_called_incorrectly) }
]

let lookup_user_op name =
  try
    find (fun o -> name = o.op_name) user_ops
  with
      Not_found -> raise Unknown_user_op

(* inspecting *)

let rec register_value_type reg =
  match reg with
      GuestRegister (_, _, value_type) -> value_type
    | HostRegister (_, value_type) -> value_type
    | IntermediateRegister expr -> expr_value_type expr
    | LetRegister (_, value_type, _) -> value_type
and expr_value_type expr =
  match expr with
    IntConst _ -> Int
  | FloatConst _ -> Float
  | ConditionConst _ -> Condition
  | Register reg -> register_value_type reg
  | LoadBO _ -> Int
  | Unary (op, _) -> unary_op_value_type op
  | UnaryWidth (op, _, _) -> unary_width_op_value_type op
  | Binary (op, _, _) -> binary_op_value_type op
  | BinaryWidth (op, _, _, _) -> binary_width_op_value_type op
  | TernaryWidth (op, _, _, _, _) -> ternary_width_op_value_type op
  | Extract _ -> Int
  | Insert _ -> Int
  | If (_, cons, alt) ->
      let cons_type = expr_value_type cons
      and alt_type = expr_value_type alt
      in if cons_type <> alt_type then raise Wrong_type ;
	cons_type
  | UserOp (name, _) -> (lookup_user_op name).result_type

let expr_sub_exprs expr =
  match expr with
    LoadBO (byte_order, width, sub) -> [ sub ]
  | Unary (unary_op, sub) -> [ sub ]
  | UnaryWidth (unary_width_op, width, sub) -> [ sub ]
  | Binary (binary_op, sub1, sub2) -> [ sub1 ; sub2 ]
  | BinaryWidth (binary_width_op, width, sub1, sub2) -> [ sub1 ; sub2 ]
  | TernaryWidth (ternary_width_op, width, sub1, sub2, sub3) -> [ sub1 ; sub2 ; sub3 ]
  | Extract (sub, start, length) -> [ sub ; start ; length ]
  | Insert (sub1, sub2, start, length) -> [ sub1 ; sub2 ; start ; length ]
  | If (sub1, sub2, sub3) -> [ sub1 ; sub2 ; sub3 ]
  | UserOp (_, subs) -> subs
  | _ -> []

let rec stmt_sub_exprs stmt =
  match stmt with
    Store (byte_order, width, sub1, sub2) -> [ sub1 ; sub2 ]
  | Assign (register, sub) -> [ sub ]
  | Let (name, _, expr, sub) -> expr :: (stmt_sub_exprs sub)
  | Seq (sub1, sub2) -> (stmt_sub_exprs sub1) @ (stmt_sub_exprs sub2)
  | IfStmt (expr, sub1, sub2) -> expr :: ((stmt_sub_exprs sub1) @ (stmt_sub_exprs sub2))

(* returns a list of tuples (reg, read, write) where reg is a GuestRegister,
   read is true if the register is read from, write is true if the register is
   written to *)
let collect_stmt_guest_regs stmt =
  let rec add_reg (reg, read, write) regs =
    match regs with
	[] -> [ (reg, read, write) ]
      | (reg1, read1, write1) :: rest when reg = reg1 ->
	  (reg, read or read1, write or write1) :: rest
      | r :: rest ->
	  r :: (add_reg (reg, read, write) rest)
  and collect_expr regs expr =
    let regs = fold_left collect_expr regs (expr_sub_exprs expr)
    in match expr with
	Register (GuestRegister _ as reg) -> add_reg (reg, true, false) regs
      | _ -> regs
  in let toplevel_regs =
      match stmt with
	  Assign (GuestRegister _ as reg, _) -> [ (reg, false, true) ]
	| _ -> []
  in fold_left collect_expr toplevel_regs (stmt_sub_exprs stmt)

let is_const expr =
    match expr with
	IntConst (IntLiteral _) | FloatConst _ | ConditionConst _ -> true
      | _ -> false

let rec expr_size expr =
    1 + (fold_left ( + ) 0 (map expr_size (expr_sub_exprs expr)))

let rec expr_fields expr =
  match expr with
      IntConst (IntField name) -> [ name ]
    | _ ->
	let sub_exprs = expr_sub_exprs expr
	in uniq (flatten (map expr_fields sub_exprs))

let stmt_fields stmt =
  uniq (flatten (map expr_fields (stmt_sub_exprs stmt)))

(* constructing *)

let int_literal_expr int =
  IntConst (IntLiteral int)

let bool_to_int_expr expr =
  If (expr, int_literal_expr one, int_literal_expr zero)

let extract_bit_expr value bit =
  Binary (BitAnd, BinaryWidth(LShiftR, 8, value, bit), int_literal_expr one)

let is_bit_set_expr value bit =
  Unary (ConditionNeg, Unary (IntEven, extract_bit_expr value bit))

let is_full_mask_expr x =
  Binary (IntEqual, x, int_literal_expr minus_one)

let is_zero_expr x =
  Binary (IntEqual, x, int_literal_expr zero)

let both_low_one_bits_expr a b =
  Binary (BothLowOneBits, a, b)

let low_mask_expr x =
  Unary (LowMask, x)

let high_mask_expr x =
  Unary (HighMask, x)

let and_expr a b =
  Binary (ConditionAnd, a, b)

let or_expr a b =
  Binary (ConditionOr, a, b)

let not_expr a =
  match a with
    Unary (ConditionNeg, x) -> x
  | _ -> Unary (ConditionNeg, a)

let bitand_expr a b =
  Binary (BitAnd, a, b)

let bitor_expr a b =
  Binary (BitOr, a, b)

let bitxor_expr a b =
  Binary (BitXor, a, b)

let bitneg_expr x =
  if expr_value_type x = Int then
    Unary (BitNeg, x)
  else
    raise Wrong_type

let shiftl_expr a b =
  Binary (ShiftL, a, b)

let bitmask_expr a b =
  Binary (BitMask, a, b)

let bitsubset_expr sub super =
  Binary (IntEqual, bitand_expr sub super, sub)

let add_expr a b =
  Binary (IntAdd, a, b)

let sub_expr a b =
  Binary (IntAdd, a, Unary (IntNeg, b))

let mul_expr a b =
  Binary (IntMul, a, b)

let sex_expr width e =
  UnaryWidth (Sex, width, e)

(* instantiation *)

let instantiate_expr expr fields_alist =
  let rec instantiate_int_const int_const =
    match int_const with
	IntLiteral _ -> int_const
      | IntField name ->
	  (try
	    IntLiteral (assq name fields_alist)
	  with
	    Not_found -> int_const)
  and instantiate expr =
    match expr with
	IntConst int_const -> IntConst (instantiate_int_const int_const)
      | FloatConst _ -> expr
      | ConditionConst _ -> expr
      | Register _ -> expr
      | LoadBO _ -> expr
      | Unary (op, arg) -> Unary (op, instantiate arg)
      | UnaryWidth (op, width, arg) -> UnaryWidth (op, width, instantiate arg)
      | Binary (op, arg1, arg2) -> Binary (op, instantiate arg1, instantiate arg2)
      | BinaryWidth (op, width, arg1, arg2) -> BinaryWidth (op, width, instantiate arg1, instantiate arg2)
      | TernaryWidth (op, width, arg1, arg2, arg3) ->
	  TernaryWidth (op, width, instantiate arg1, instantiate arg2, instantiate arg3)
      | Extract (arg, start, length) ->
	  Extract (instantiate arg, instantiate start, instantiate length)
      | Insert (arg1, arg2, start, length) ->
	  Insert (instantiate arg1, instantiate arg2, instantiate start, instantiate length)
      | If (arg1, arg2, arg3) ->
	  If (instantiate arg1, instantiate arg2, instantiate arg3)
      | UserOp (name, subs) ->
	  UserOp (name, map instantiate subs)
  in
    instantiate expr

(* applying a modifier to all subexpressions *)

let apply_to_expr_subs_with_monad return bind modify expr =
  let bind2 = make_bind2 bind bind
  and bind3 = make_bind3 bind bind bind
  in let rec apply expr =
    match expr with
	IntConst _ -> return expr
      | FloatConst _ -> return expr
      | ConditionConst _ -> return expr
      | Register _ -> return expr
      | LoadBO (bo, width, addr) ->
	  bind (modify addr)
	    (fun maddr -> return (LoadBO (bo, width, maddr)))
      | Unary (op, arg) ->
	  bind (modify arg)
	    (fun marg -> return (Unary (op, marg)))
      | UnaryWidth (op, width, arg) ->
	  bind (modify arg)
	    (fun marg -> return (UnaryWidth (op, width, marg)))
      | Binary (op, arg1, arg2) ->
	  bind2 (modify arg1) (modify arg2)
	    (fun marg1 marg2 -> return (Binary (op, marg1, marg2)))
      | BinaryWidth (op, width, arg1, arg2) ->
	  bind2 (modify arg1) (modify arg2)
	    (fun marg1 marg2 -> return (BinaryWidth (op, width, marg1, marg2)))
      | TernaryWidth (op, width, arg1, arg2, arg3) ->
	  bind3 (modify arg1) (modify arg2) (modify arg3)
	    (fun marg1 marg2 marg3 -> return (TernaryWidth (op, width, marg1, marg2, marg3)))
      | Extract (arg, start, length) ->
	  bind3 (modify arg) (modify start) (modify length)
	    (fun marg mstart mlength -> return (Extract (marg, mstart, mlength)))
      | Insert (arg1, arg2, start, length) ->
	  bind2 (modify arg1) (modify arg2)
	    (fun marg1 marg2 ->
	       bind2 (modify start) (modify length)
	         (fun mstart mlength -> return (Insert (marg1, marg2, mstart, mlength))))
      | If (condition, cons, alt) ->
	  bind3 (modify condition) (modify cons) (modify alt)
	    (fun mcondition mcons malt -> return (If (mcondition, mcons, malt)))
      | UserOp (name, subs) ->
	  match subs with
	      [] -> return (UserOp (name, []))
	    | [a] ->
		bind (modify a)
		  (fun ma -> return (UserOp (name, [ma])))
	    | [a ; b] ->
		bind2 (modify a) (modify b)
		  (fun ma mb -> return (UserOp (name, [ma ; mb])))
	    | [a ; b ; c ] ->
		bind3 (modify a) (modify b) (modify c)
		  (fun ma mb mc -> return (UserOp (name, [ma ; mb ; mc])))
	    | _ ->
		raise User_op_args_unhandled
  in apply expr

let apply_to_expr_subs =
  apply_to_expr_subs_with_monad (fun x -> x) (fun v f -> f v)

let apply_to_stmt_subs_with_monad return bind modify stmt =
  let rec apply stmt =
    match stmt with
	Store (byte_order, width, addr, value) ->
	  (make_bind2 bind bind) (modify addr) (modify value)
	  (fun maddr mvalue ->
	     return (Stmt (Store (byte_order, width, get_expr maddr, get_expr mvalue))))
      | Assign (register, expr) ->
	  bind (modify expr)
	    (fun mexpr ->
	       return (Stmt (Assign (register, get_expr mexpr))))
      | Let (name, width, expr, sub) ->
	  (make_bind2 bind bind)
            (modify expr)
            (apply sub)
            (fun mexpr msub ->
	       return (Stmt (Let (name, width, get_expr mexpr, get_stmt msub))))
      | Seq (sub1, sub2) ->
	  (make_bind2 bind bind)
	    (apply sub1)
	    (apply sub2)
	    (fun msub1 msub2 ->
	       return (Stmt (Seq (get_stmt msub1, get_stmt msub2))))
      | IfStmt (expr, sub1, sub2) ->
	  (make_bind3 bind bind bind)
	    (modify expr)
	    (apply sub1)
	    (apply sub2)
	    (fun mexpr msub1 msub2 ->
	       return (Stmt (IfStmt (get_expr mexpr, get_stmt msub1, get_stmt msub2))))
  in apply stmt

let apply_to_stmt_subs modify stmt =
  get_stmt (apply_to_stmt_subs_with_monad (fun x -> x) (fun v f -> f v) modify stmt)

(* constant folding *)

let cfold_expr fields expr =
  let rec apply_int_unary op arg =
    match op with
	LoadByte -> Unary (LoadByte, IntConst (IntLiteral arg))
      | IntToFloat -> FloatConst (to_float arg)
      | IntEven -> ConditionConst ((rem arg 2L) = 0L)
      | IntNeg -> IntConst (IntLiteral (neg arg))
      | BitNeg -> IntConst (IntLiteral (lognot arg))
      | LowOneBits -> int_literal_expr (low_one_bits arg)
      | LowMask -> int_literal_expr (low_mask arg)
      | HighMask -> int_literal_expr (high_mask arg)
      | _ -> raise Wrong_type
  and apply_float_unary op arg =
    match op with
	FloatToInt -> IntConst (IntLiteral (of_float arg))
      | FloatSqrt -> FloatConst (sqrt arg)
      | FloatNeg -> FloatConst (~-. arg)
      | FloatAbs -> FloatConst (abs_float arg)
      | _ -> raise Wrong_type
  and apply_condition_unary op arg =
    match op with
	ConditionToInt -> IntConst (IntLiteral (if arg then 1L else 0L))
      | ConditionNeg -> ConditionConst (not arg)
      | _ -> raise Wrong_type
  and mask_int width arg =
    logand (width_mask width) arg
  and apply_int_unary_width op width arg =
    let arg = mask_int width arg
    in match op with
	IntParityEven -> ConditionConst ((int_parity arg) = 0L)
      | IntSign -> ConditionConst ((shift_right_logical arg (width * 8 - 1)) <> 0L)
      | Sex -> IntConst (IntLiteral (sex (width * 8) arg))
      | Zex -> IntConst (IntLiteral arg)
  and apply_int_binary op arg1 arg2 =
    match op with
        IntEqual -> ConditionConst (arg1 = arg2)
      | LessU -> ConditionConst (less_u arg1 arg2)
      | LessS -> ConditionConst (less_s arg1 arg2)
      | IntAdd -> IntConst (IntLiteral (add arg1 arg2))
      | IntMul -> IntConst (IntLiteral (mul arg1 arg2))
      | BitAnd -> IntConst (IntLiteral (logand arg1 arg2))
      | BitOr -> IntConst (IntLiteral (logor arg1 arg2))
      | BitXor -> IntConst (IntLiteral (logxor arg1 arg2))
      | ShiftL -> IntConst (IntLiteral (shiftl arg1 arg2))
      | BothLowOneBits -> int_literal_expr (both_low_one_bits arg1 arg2)
      | BitMask -> int_literal_expr (bitmask arg1 arg2)
      | _ -> raise Wrong_type
  and apply_float_binary op arg1 arg2 =
    match op with
	FloatEqual -> ConditionConst (arg1 = arg2)
      | FloatLess -> ConditionConst (arg1 < arg2)
      | FloatAdd -> FloatConst (arg1 +. arg2)
      | FloatSub -> FloatConst (arg1 -. arg2)
      | FloatMul -> FloatConst (arg1 *. arg2)
      | FloatDiv -> FloatConst (arg1 /. arg2)
      | _ -> raise Wrong_type
  and apply_condition_binary op arg1 arg2 =
    match op with
	ConditionAnd -> ConditionConst (arg1 && arg2)
      | ConditionOr -> ConditionConst (arg1 || arg2)
      | ConditionXor -> ConditionConst ((arg1 && (not arg2)) || ((not arg1) && arg2))
      | _ -> raise Wrong_type
  and apply_int_binary_width op width arg1 arg2 =
    let arg1 = mask_int width arg1
    and arg2 = mask_int width arg2
    in match op with
      AddCarry ->
	if width > 4 then raise Unsupported_width ;
	ConditionConst (is_bit_set (add arg1 arg2) (width * 8))
    | SubCarry ->
	if width > 4 then raise Unsupported_width ;
	ConditionConst (is_bit_set (sub arg1 arg2) (width * 8))
    | Overflow ->
	let overflow_bit = (width * 8) - 1
	in if (is_bit_set arg1 overflow_bit) = (is_bit_set arg2 overflow_bit) then
	  ConditionConst ((is_bit_set (add arg1 arg2) overflow_bit) <> (is_bit_set arg1 overflow_bit))
	else
	  ConditionConst false
    | LShiftR -> IntConst (IntLiteral (lshiftr arg1 arg2))
    | AShiftR ->
	let amount = if ((compare arg2 (mul 8L (of_int width))) >= 0) || ((compare arg2 0L) < 0) then
	  8 * width - 1
	else
	  to_int arg2
	in 
	  IntConst (IntLiteral (shift_right (shift_left arg1 (64 - (8 * width))) ((64 - (8 * width)) + amount)))
    | IntMulHS ->
	if width > 4 then raise Unsupported_width ;
	let arg1 = sex (width * 8) arg1
	and arg2 = sex (width * 8) arg2
	in IntConst (IntLiteral (shift_right (mul arg1 arg2) (width * 8)))
    | IntMulHU ->
	if width > 4 then raise Unsupported_width ;
	IntConst (IntLiteral (shift_right (mul arg1 arg2) (width * 8)))
  and apply_int_ternary_width op width arg1 arg2 arg3 =
    raise Unsupported_operation
  and cfold expr =
    let fexpr = apply_to_expr_subs cfold expr
    in match fexpr with
      IntConst (IntField input_name) ->
	(try
	   IntConst (IntLiteral (assoc input_name fields))
	 with
	     Not_found -> expr)
    | IntConst _ -> expr
    | FloatConst _ -> expr
    | ConditionConst _ -> expr
    | Register _ -> expr
    | LoadBO _ -> expr
    | Unary (op, arg) ->
	(match arg with
	  IntConst (IntLiteral int) -> apply_int_unary op int
	| FloatConst float -> apply_float_unary op float
	| ConditionConst condition -> apply_condition_unary op condition
	| _ -> fexpr)
    | UnaryWidth (op, width, arg) ->
	(match arg with
	  IntConst (IntLiteral int64) -> apply_int_unary_width op width int64
	| FloatConst float -> raise Wrong_type
	| _ -> fexpr)
    | Binary (op, arg1, arg2) ->
	(match (arg1, arg2) with
	  (IntConst (IntLiteral int1), IntConst (IntLiteral int2)) -> apply_int_binary op int1 int2
	| (FloatConst float1, FloatConst float2) -> apply_float_binary op float1 float2
	| (ConditionConst cond1, ConditionConst cond2) -> apply_condition_binary op cond1 cond2
	| _ -> fexpr)
    | BinaryWidth (op, width, arg1, arg2) ->
	(match (arg1, arg2) with
	  (IntConst (IntLiteral int1), IntConst (IntLiteral int2)) -> apply_int_binary_width op width int1 int2
	| _ -> fexpr)
    | TernaryWidth (op, width, arg1, arg2, arg3) ->
	(match (arg1, arg2, arg3) with
	  (IntConst (IntLiteral int1), IntConst (IntLiteral int2), IntConst (IntLiteral int3)) ->
	    IntConst (IntLiteral (apply_int_ternary_width op width int1 int2 int3))
	| _ -> fexpr)
    | Extract (arg, start, length) ->
	(match (arg, start, length) with
	  (IntConst (IntLiteral int), IntConst (IntLiteral start_int), IntConst (IntLiteral length_int)) ->
	    IntConst (IntLiteral (extract_bits int start_int length_int))
	| _ -> fexpr)
    | Insert (arg1, arg2, start, length) ->
	(match (arg1, arg2, start, length) with
	  (IntConst (IntLiteral int1), IntConst (IntLiteral int2),
	   IntConst (IntLiteral start_int), IntConst (IntLiteral length_int)) ->
	    IntConst (IntLiteral (insert_bits int1 int2 start_int length_int))
	| _ -> fexpr)
    | If (condition, cons, alt) ->
	(match condition with
	  ConditionConst true -> cons
	| ConditionConst false -> alt
	| _ -> fexpr)
    | UserOp (name, args) ->
	let cargs = map cfold args
	and user_op = lookup_user_op name
	in if ((length cargs) = user_op.num_args) then
	    try
	      let iargs = map (fun e -> match e with IntConst (IntLiteral i) -> i | _ -> raise Expression_not_const) cargs
	      in user_op.apply_op iargs
	    with
		Expression_not_const -> UserOp (name, cargs)
	  else
	    raise Wrong_user_op_args
  in
    cfold expr

let cfold_stmt fields =
  apply_to_stmt_subs (fun expr -> Expr (cfold_expr fields expr))

(* an expression is register const if its value only depends on fields, but
   not on registers or memory *)
let rec is_register_const fields expr =
  match expr with
      Register _ -> false
    | LoadBO _ -> false
    | Unary (LoadByte, _) -> false
    (*
    | If (cond, cons, alt) ->
	(match cfold_expr fields cond with
	     ConditionConst true -> is_register_const fields cons
	   | ConditionConst false -> is_register_const fields alt
	   | _ -> (is_register_const fields cons) && (is_register_const fields alt))
    *)
    | _ -> for_all (is_register_const fields) (expr_sub_exprs expr)

(* printing *)

let value_type_string value_type =
  match value_type with
      Int -> "I"
    | Float -> "F"
    | Condition -> "C"

let byte_order_string byte_order =
  match byte_order with
    BigEndian -> "BE"
  | LittleEndian -> "LE"

let print_width width =
  print_int width

let print_precision precision =
  match precision with
    Single -> print_string "S"
  | Double -> print_string "D"

let print_byte_order byte_order =
  print_string (byte_order_string byte_order)

let print_value_type value_type =
  print_string (value_type_string value_type)

let print_input_name input_name =
  print_string "?" ; print_string input_name

let print_int_const const =
  match const with
      IntLiteral int -> print_string (to_string int)
    | IntField input_name -> print_input_name input_name

let print_register register =
  match register with
      GuestRegister (_, num, value_type) -> print_string "G" ; print_value_type value_type ; print_int_const num
    | HostRegister (num, value_type) -> print_string "H" ; print_value_type value_type ; print_int num
    | IntermediateRegister expr -> print_string "*IR*"
    | LetRegister (name, value_type, _) -> print_string "L" ; print_string name

let rec print_expr expr =
  let print_args exprs =
    match exprs with
	[] -> ()
      | [e] -> print_expr e
      | e :: rest -> print_expr e ; print_string ","
  in match expr with
      IntConst const -> print_int_const const
    | FloatConst const -> print_float const
    | ConditionConst false -> print_string "False"
    | ConditionConst true -> print_string "True"
    | Register register -> print_register register
    | LoadBO (byte_order, width, addr) ->
	print_string "MEM" ; print_width width ; print_byte_order byte_order ;
	print_string "(" ; print_expr addr ; print_string ")"
    | Unary (op, arg) ->
	print_string (unary_op_name op) ; print_string "(" ; print_expr arg ; print_string ")"
    | UnaryWidth (op, width, arg) ->
	print_string (unary_width_op_name op) ; print_width width ; print_string "(" ; print_expr arg ; print_string ")"
    | Binary (op, arg1, arg2) ->
	print_string (binary_op_name op) ; print_string "(" ; print_expr arg1 ;
	print_string ", " ; print_expr arg2 ; print_string ")"
    | BinaryWidth (op, width, arg1, arg2) ->
	print_string (binary_width_op_name op) ; print_width width ; print_string "(" ; print_expr arg1 ;
	print_string ", " ; print_expr arg2 ; print_string ")"
    | TernaryWidth (op, width, arg1, arg2, arg3) ->
	print_string (ternary_width_op_name op) ; print_width width ; print_string "(" ; print_expr arg1 ;
	print_string ", " ; print_expr arg2 ; print_string ", " ; print_expr arg3 ; print_string ")"
    | Extract (expr, start, length) ->
	print_string "Extract(" ; print_expr expr ; print_string ", " ; print_expr start ;
	print_string ", " ; print_expr length ; print_string ")"
    | Insert (arg1, arg2, start, length) ->
	print_string "Insert(" ; print_expr arg1 ; print_string ", " ; print_expr arg2 ;
	print_string ", " ; print_expr start ; print_string ", " ; print_expr length ; print_string ")"
    | If (condition, cons, alt) ->
	print_string "If(" ; print_expr condition ; print_string ", " ; print_expr cons ;
	print_string ", "; print_expr alt ; print_string ")"
    | UserOp (name, args) ->
	print_string (name ^ "(") ; print_args args ; print_string ")"

let rec print_stmt stmt =
  match stmt with
    Store (byte_order, width, addr, value) ->
      print_string "MEM" ; print_width width ; print_byte_order byte_order ;
      print_string "[" ; print_expr addr ; print_string "] := " ; print_expr value ; print_newline ()
  | Assign (dst, src) -> print_register dst ; print_string " := " ; print_expr src ; print_newline ()
  | Let (name, width, expr, sub) ->
      print_string (name ^ " :=") ; print_int width ; print_string " " ;
      print_expr expr ; print_string ";\n" ; print_stmt sub
  | Seq (sub1, sub2) -> print_stmt sub1 ; print_string ";\n" ; print_stmt sub2
  | IfStmt (expr, sub1, sub2) -> print_string "If " ; print_expr expr ; print_string " then\n" ; print_stmt sub1 ;
      print_string "else\n" ; print_stmt sub2 ; print_string "endif\n"

(*** patterns ***)

(* types *)

type width_pattern = (int list) * input_name

type int_pattern =
    AnyInt of input_name
  | TheInt of expr

type float_pattern =
    AnyFloat of input_name
  | TheFloat of float

type bool_pattern =
    AnyBool of input_name
  | TheBool of bool

type pattern =
    IntPattern of int_pattern
  | FloatPattern of float_pattern
  | ConditionPattern of bool_pattern
  | ExprPattern of input_name
  | RegisterPattern of input_name
  | LoadBOPattern of byte_order * width_pattern * pattern
  | UnaryPattern of unary_op * pattern
  | UnaryWidthPattern of unary_width_op * width_pattern * pattern
  | BinaryPattern of binary_op * pattern * pattern
  | BinaryWidthPattern of binary_width_op * width_pattern * pattern * pattern
  | TernaryWidthPattern of ternary_width_op * width_pattern * pattern * pattern * pattern
  | ExtractPattern of pattern * int_pattern * int_pattern
  | InsertPattern of pattern * pattern * int_pattern * int_pattern
  | IfPattern of pattern * pattern * pattern

type stmt_pattern =
    StorePattern of byte_order * width_pattern * pattern * pattern
  | AssignPattern of input_name * pattern

(* printing *)

let print_width_pattern (widths, input_name) =
  print_string "(" ; iter (fun w -> print_int w) widths ; print_string ")"

let print_int_pattern pattern =
  match pattern with
      AnyInt input_name -> print_input_name input_name
    | TheInt expr -> print_expr expr

let print_float_pattern pattern =
  match pattern with
      AnyFloat input_name -> print_input_name input_name
    | TheFloat float -> print_float float

let print_condition_pattern pattern =
  match pattern with
      AnyBool input_name -> print_input_name input_name
    | TheBool bool -> print_string (if bool then "True" else "False")

let rec print_pattern pattern =
  match pattern with
    IntPattern int_pattern -> print_int_pattern int_pattern
  | FloatPattern float_pattern -> print_float_pattern float_pattern
  | ConditionPattern condition_pattern -> print_condition_pattern condition_pattern
  | ExprPattern input_name -> print_input_name input_name
  | RegisterPattern input_name -> print_input_name input_name
  | LoadBOPattern (byte_order, width, addr) ->
      print_string "MEM" ; print_width_pattern width ; print_byte_order byte_order ;
      print_string "(" ; print_pattern addr ; print_string ")"
  | UnaryPattern (op, arg) ->
      print_string (unary_op_name op) ; print_string "(" ; print_pattern arg ; print_string ")"
  | UnaryWidthPattern (op, width_pattern, arg) ->
      print_string (unary_width_op_name op) ; print_width_pattern width_pattern ;
      print_string "(" ; print_pattern arg ; print_string ")"
  | BinaryPattern (op, arg1, arg2) ->
      print_string (binary_op_name op) ; print_string "(" ; print_pattern arg1 ;
      print_string ", " ; print_pattern arg2 ; print_string ")"
  | BinaryWidthPattern (op, width, arg1, arg2) ->
      print_string (binary_width_op_name op) ; print_width_pattern width ; print_string "(" ; print_pattern arg1 ;
      print_string ", " ; print_pattern arg2 ; print_string ")"
  | TernaryWidthPattern (op, width, arg1, arg2, arg3) ->
      print_string (ternary_width_op_name op) ; print_width_pattern width ; print_string "(" ; print_pattern arg1 ;
      print_string ", " ; print_pattern arg2 ; print_string ", " ; print_pattern arg3 ; print_string ")"
  | ExtractPattern (arg, start, length) ->
      print_string "Extract(" ; print_pattern arg ; print_string ", " ; print_int_pattern start ;
      print_string ", " ; print_int_pattern length ; print_string ")"
  | InsertPattern (arg1, arg2, start, length) ->
      print_string "Insert(" ; print_pattern arg1 ; print_string ", " ; print_pattern arg2 ;
      print_string ", " ; print_int_pattern start ; print_string ", " ; print_int_pattern length ; print_string ")"
  | IfPattern (cond, cons, alt) ->
      print_string "If(" ; print_pattern cond ; print_string ", " ; print_pattern cons ;
      print_string ", " ; print_pattern alt ; print_string ")"

let print_stmt_pattern pattern =
  match pattern with
    StorePattern (byte_order, width_pattern, addr, value) ->
      print_string "MEM" ; print_width_pattern width_pattern ; print_byte_order byte_order ;
      print_string "[" ; print_pattern addr ; print_string "] := " ; print_pattern value ; print_newline ()
  | AssignPattern (dst, src) -> print_input_name dst ; print_string " := " ; print_pattern src ; print_newline ()
