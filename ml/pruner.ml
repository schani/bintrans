open Int64

open Bitmath
open Expr
open Machine

let rec low_one_bits int =
  if is_bit_set int 0 then
    logor (shift_left (low_one_bits (shift_right_logical int 1)) 1) one
  else
    zero

let both_low_one_bits arg1 arg2 =
  logand (low_one_bits arg1) (low_one_bits arg2)

let rec low_mask int =
  if int = zero then
    zero
  else
    logor (shift_left (low_mask (shift_right_logical int 1)) 1) one

let rec high_mask int =
  if int = zero then
    zero
  else
    logor (shift_right_logical (high_mask (shift_left int 1)) 1) (shift_left one 63)

let bool_to_int bool =
  if bool then one else zero

(* Our policy here is that we assume that constant folding and
simplification has already taken place, so in cases where we'd have to
know all bits to of an operand to know any bit of the result, we don't
bother to check, because if all bits of the operand are known, it's a
constant and hence the operation in question would have been folded
away.  *)
let rec known expr =
  let known_unary op arg =
    match op with
	LoadByte -> zero
      | IntToFloat | FloatToInt
      | ConditionToInt ->
	  if is_bit_set (known arg) 0 then minus_one else zero
      | IntEven -> bool_to_int (is_bit_set (known arg) 0)
      | IntNeg -> low_one_bits (known arg)
      | BitNeg -> known arg
      | ConditionNeg -> known arg
      | FloatSqrt | FloatNeg | FloatAbs -> zero
  and known_unary_width op width arg =
    match op with
	IntZero | IntParityEven ->
	  let mask = width_mask width
	  in let karg = logand mask (known arg)
	  in bool_to_int (karg = mask)
      | IntSign -> bool_to_int (is_bit_set (known arg) (width * 8 - 1))
      | Sex -> sex (width * 8) (known arg)
      | Zex -> zex (width * 8) (known arg)
  and known_binary op arg1 arg2 =
    match op with
	FloatEqual | FloatLess -> zero
      | IntAdd | IntSub | IntMul -> both_low_one_bits (known arg1) (known arg2)
      | BitAnd | ConditionAnd ->
	  let karg1 = known arg1
	  and karg2 = known arg2
	  and barg1 = bits arg1
	  and barg2 = bits arg2
	  in logor (logand karg1 karg2)
	    (logor (logand karg1 (lognot barg1))
	       (logand karg2 (lognot barg2)))
      | BitOr | ConditionOr ->
	  logor (logand (known arg1) (known arg2))
	    (logor (bits arg1) (bits arg2))
      | BitXor | ConditionXor -> logand (known arg1) (known arg2)
      | ShiftL ->
	  (match arg2 with
	    IntConst (IntLiteral amount) -> shiftl (known arg1) amount
	  | _ -> zero)
      | FloatAdd | FloatSub | FloatMul | FloatDiv -> zero
  and known_binary_width op width arg1 arg2 =
    let mask = width_mask width
    in let karg1 = logand mask (known arg1)
       and karg2 = logand mask (known arg2)
    in let all_known = karg1 = mask && karg2 = mask
    in match op with
      IntEqual ->
	let both_known = logand karg1 karg2
	in bool_to_int (all_known
		      || ((logand (bits arg1) both_known) <> (logand (bits arg2) both_known)))
    | LessU | LessS | AddCarry | SubCarry | Overflow ->
	bool_to_int all_known
    | LShiftR ->
	(match arg2 with
	  IntConst (IntLiteral amount) ->
	    (logor (lshiftr (known arg1) (logand amount mask))
	       (bitmask (sub 64L amount) amount))
	| _ -> zero)
    | AShiftR ->
	(match arg2 with
	  IntConst (IntLiteral amount) ->
	    logand (ashiftr (sex (width * 8) (known arg1)) (logand amount mask)) mask
	| _ -> zero)
    | IntMulHS | IntMulHU -> zero
  and known_ternary_width op width arg1 arg2 arg3 =
    zero
  in match expr with
    IntConst (IntLiteral _) -> minus_one
  | IntConst _ -> zero
  | FloatConst _ -> minus_one
  | ConditionConst _ -> minus_one
  | Register _ -> zero
  | LoadBO _ -> zero
  | Unary (op, arg) -> known_unary op arg
  | UnaryWidth (op, width, arg) -> known_unary_width op width arg
  | Binary (op, arg1, arg2) -> known_binary op arg1 arg2
  | BinaryWidth (op, width, arg1, arg2) -> known_binary_width op width arg1 arg2
  | TernaryWidth (op, width, arg1, arg2, arg3) -> known_ternary_width op width arg1 arg2 arg3
  | Extract (arg, start, length) ->
      (let karg = known arg
      in match (start, length) with
	(IntLiteral start, IntLiteral length) ->
	  (logor (extract_bits karg start length)
	     (bitmask length (sub 64L length)))
      | (_, IntLiteral length) -> bitmask length (sub 64L length)
      | _ -> zero)
  | Insert (arg1, arg2, start, length) ->
      (let karg1 = known arg1
      and karg2 = known arg2
      in match (start, length) with
	(IntLiteral start, IntLiteral length) ->
	  insert_bits karg1 karg2 start length
      | (IntLiteral start, _) ->
	  logand karg1 (bitmask 0L start)
      | _ -> zero)
  | If (condition, cons, alt) ->
      let kcons = known cons
      and kalt = known alt
      in logand kcons kalt
and bits expr =
  let bits_unary op arg =
    match op with
      LoadByte -> zero
    | IntToFloat | FloatToInt -> zero
    | ConditionToInt -> bool_to_int ((is_bit_set (known arg) 0)
				       && (is_bit_set (bits arg) 0))
    | IntEven -> bool_to_int ((is_bit_set (known arg) 0)
				&& (not (is_bit_set (bits arg) 0)))
    | IntNeg ->
	logand (neg (bits arg)) (known expr)
    | BitNeg | ConditionNeg ->
	logand (lognot (bits arg)) (known expr)
    | FloatSqrt | FloatNeg | FloatAbs -> zero
  and bits_unary_width op width arg =
    let mask = width_mask width
    in match op with
      IntZero -> bool_to_int ((is_bit_set (known expr) 0)
				&& ((logand (bits arg) mask) = 0L))
    | IntParityEven -> bool_to_int ((is_bit_set (known expr) 0)
				      && ((int_parity (logand (bits arg) mask)) = 0L))
    | IntSign ->
	let sign_bit = width * 8 - 1
	in if is_bit_set (known arg) sign_bit then
	  extract_bits (bits arg) (of_int sign_bit) 1L
	else
	  zero
    | Sex -> sex (width * 8) (bits arg)
    | Zex -> zex (width * 8) (bits arg)
  and bits_binary op arg1 arg2 =
    match op with
      FloatEqual | FloatLess -> zero
    | IntAdd ->
	logand (add (bits arg1) (bits arg2)) (both_low_one_bits (known arg1) (known arg2))
    | IntSub ->
	logand (sub (bits arg1) (bits arg2)) (both_low_one_bits (known arg1) (known arg2))
    | IntMul ->
	logand (mul (bits arg1) (bits arg2)) (both_low_one_bits (known arg1) (known arg2))
    | BitAnd | ConditionAnd -> logand (bits arg1) (bits arg2)
    | BitOr | ConditionOr -> logor (bits arg1) (bits arg2)
    | BitXor | ConditionXor -> logxor (bits arg1) (bits arg2)
    | ShiftL ->
	(match arg2 with
	  IntConst (IntLiteral amount) -> shiftl (bits arg1) amount
	| _ -> zero)
    | FloatAdd | FloatSub | FloatMul | FloatDiv -> zero
  and bits_binary_width op width arg1 arg2 =
    let mask = width_mask width
    and bit_width = width * 8
    in let karg1 = logand mask (known arg1)
    and karg2 = logand mask (known arg2)
    and barg1 = logand mask (bits arg1)
    and barg2 = logand mask (bits arg2)
    in let all_known = karg1 = mask && karg2 = mask
    in match op with
      IntEqual -> bool_to_int (all_known && (barg1 = barg2))
    | LessU -> bool_to_int (all_known && (less_u barg1 barg2))
    | LessS -> bool_to_int (all_known && (less_s (sex bit_width barg1) (sex bit_width barg2)))
    | AddCarry ->
	if width > 4 then raise Unsupported_width ;
	bool_to_int (all_known && (is_bit_set (add barg1 barg2) (width * 8)))
    | SubCarry ->
	if width > 4 then raise Unsupported_width ;
	bool_to_int (all_known && (is_bit_set (sub barg1 barg2) (width * 8)))
    | Overflow ->
	if all_known then
	  (let overflow_bit = (width * 8) - 1
	  in if (is_bit_set barg1 overflow_bit) = (is_bit_set barg2 overflow_bit) then
	    bool_to_int ((is_bit_set (add barg1 barg2) overflow_bit) <> (is_bit_set barg1 overflow_bit))
	  else
	    zero)
	else
	  zero
    | LShiftR ->
	(match arg2 with
	  IntConst (IntLiteral amount) -> lshiftr barg1 (logand amount mask)
	| _ -> zero)
    | AShiftR ->
	(match arg2 with
	  IntConst (IntLiteral amount) ->
	    logand (ashiftr (sex bit_width barg1) (logand amount mask)) mask
	| _ -> zero)
    | IntMulHS | IntMulHU -> zero
  and bits_ternary_width op width arg1 arg2 arg3 =
    zero
  in match expr with
    IntConst (IntLiteral int) -> int
  | IntConst _ -> zero
  | FloatConst float -> bits_of_float float
  | ConditionConst bool -> bool_to_int bool
  | Register _ -> zero
  | LoadBO _ -> zero
  | Unary (op, arg) -> bits_unary op arg
  | UnaryWidth (op, width, arg) -> bits_unary_width op width arg
  | Binary (op, arg1, arg2) -> bits_binary op arg1 arg2
  | BinaryWidth (op, width, arg1, arg2) -> bits_binary_width op width arg1 arg2
  | TernaryWidth (op, width, arg1, arg2, arg3) -> bits_ternary_width op width arg1 arg2 arg3
  | Extract (arg, start, length) ->
      (match (start, length) with
	(IntLiteral start, IntLiteral length) ->
	  extract_bits (bits arg) start length
      | (_, IntLiteral length) -> zero
      | _ -> zero)
  | Insert (arg1, arg2, start, length) ->
      (match (start, length) with
	(IntLiteral start, IntLiteral length) ->
	  insert_bits (bits arg1) (bits arg2) start length
      | (IntLiteral start, _) ->
	  logand (bits arg1) (bitmask 0L start)
      | _ -> zero)
  | If (condition, cons, alt) ->
      logand (bits cons) (bits alt)

let rec prune expr needed =
  if (logand (known expr) needed) = needed then
    let bits = bits expr
    in match expr_value_type expr with
      Int -> IntConst (IntLiteral bits)
    | Float -> raise Wrong_type
    | Condition -> ConditionConst (is_bit_set bits 0)
  else
    let prune_unary op arg =
      match op with
	  LoadByte | IntToFloat | FloatToInt | FloatSqrt | FloatNeg | FloatAbs ->
	    Unary (op, prune arg minus_one)
	| ConditionToInt ->
	    if not (is_bit_set needed 0) then
	      IntConst (IntLiteral zero)
	    else if is_bit_set (known arg) 0 then
	      IntConst (IntLiteral (logand (bits arg) 1L))
	    else
	      Unary (ConditionToInt, prune arg 1L)
	| IntEven ->
	    if is_bit_set (known arg) 0 then
	      ConditionConst (not (is_bit_set (bits arg) 0))
	    else
	      Unary (IntEven, prune arg 1L)
	| IntNeg ->
	    Unary (IntNeg, prune arg (low_mask needed))
	| BitNeg | ConditionNeg ->
	    Unary (op, prune arg needed)
    and prune_unary_width op width arg =
      match op with
	IntZero | IntParityEven | Sex | Zex ->
	  UnaryWidth (op, width, prune arg (width_mask width))
      | IntSign -> 
	  UnaryWidth (op, width, prune arg (bitmask (of_int (width * 8 - 1)) 1L))
    and prune_binary op arg1 arg2 =
      match op with
	  FloatEqual | FloatLess | FloatAdd | FloatSub | FloatMul | FloatDiv ->
	    Binary (op, prune arg1 minus_one, prune arg2 minus_one)
	| IntAdd | IntSub | IntMul ->
	    Binary (op, prune arg1 (low_mask needed), prune arg2 (low_mask needed))
	| BitAnd | ConditionAnd ->
	    let karg1 = known arg1
	    and karg2 = known arg2
	    and barg1 = bits arg1
	    and barg2 = bits arg2
	    in if (logand (logand karg1 barg1) needed) = needed then
		prune arg2 needed
	      else if (logand (logand karg2 barg2) needed) = needed then
		prune arg1 needed
	      else
		Binary (op,
			prune arg1 (logand needed (lognot (logand karg2 (lognot barg2)))),
			prune arg2 (logand needed (lognot (logand karg1 (lognot barg1)))))
	| BitOr | ConditionOr ->
	    let karg1 = known arg1
	    and karg2 = known arg2
	    and barg1 = bits arg1
	    and barg2 = bits arg2
	    in if (logand karg1 needed) = needed && (logand barg1 needed) = zero then
		prune arg2 needed
	      else if (logand karg2 needed) = needed && (logand barg2 needed) = zero then
		prune arg1 needed
	      else
		Binary (BitOr,
			prune arg1 (logand needed (lognot (logand karg2 barg2))),
			prune arg2 (logand needed (lognot (logand karg1 barg1))))
	| BitXor | ConditionXor->
	    let karg1 = known arg1
	    and karg2 = known arg2
	    and barg1 = bits arg1
	    and barg2 = bits arg2
	    in if ((logand karg1 needed) = needed && (logand barg1 needed) = zero) then
		prune arg2 needed
	      else if ((logand karg2 needed) = needed && (logand barg2 needed) = zero) then
		prune arg1 needed
	      else
		Binary (BitXor, prune arg1 needed, prune arg2 needed)
	| ShiftL ->
	    let karg2 = known arg2
	    and barg2 = bits arg2
	    in if (logand karg2 0x3fL) = 0x3fL then
		(if (logand barg2 0x3fL) = zero then
		   prune arg1 needed
		 else
		   Binary (ShiftL, prune arg1 (lshiftr needed (logand barg2 0x3fL)), prune arg2 0x3fL))
	      else
		Binary (ShiftL, prune arg1 (low_mask needed), prune arg2 0x3fL)
    and prune_binary_width op width arg1 arg2 =
      let mask = width_mask width
      and shift_mask = width_shift_mask width
      in match op with
	  IntEqual | LessU | LessS | AddCarry | SubCarry | Overflow ->
	    if is_bit_set (known expr) 0 then
	      ConditionConst (is_bit_set (bits expr) 0)
	    else
	      BinaryWidth (op, width, prune arg1 mask, prune arg2 mask)
	| LShiftR ->
	    let shift_mask = width_shift_mask width
	    and barg2 = bits arg2
	    in if (logand (known arg2) shift_mask) = shift_mask then
		(if (logand barg2 shift_mask) = zero then
		   prune arg1 (logand needed mask)
		 else
		   BinaryWidth (LShiftR, width,
				prune arg1 (logand (shiftl needed (logand barg2 shift_mask)) mask),
				prune arg2 shift_mask))
	      else
		BinaryWidth (LShiftR, width, prune arg1 (logand (high_mask needed) mask), prune arg2 shift_mask)
	| AShiftR ->
	    let shift_mask = width_shift_mask width
	    and barg2 = bits arg2
	    in if (logand (known arg2) shift_mask) = shift_mask then
		(if (logand barg2 shift_mask) = zero then
		   prune arg1 (logand needed mask)
		 else
		   let amount = logand barg2 shift_mask
		   in let high_bit_mask = logand (lognot (lshiftr mask amount)) mask
		   in let need_high_bit = (logand needed high_bit_mask) <> zero
		   in let high_bit = if need_high_bit then (bitmask (of_int (width * 8 - 1)) one) else zero
		   in BinaryWidth (AShiftR, width,
				   prune arg1 (logor high_bit (logand (shiftl needed (logand barg2 shift_mask)) mask)),
				   prune arg2 shift_mask))
	      else
		BinaryWidth (AShiftR, width, prune arg1 (logand (high_mask needed) mask), prune arg2 shift_mask)
	| IntMulHS | IntMulHU ->
	    BinaryWidth (op, width, prune arg1 mask, prune arg2 mask)
    and prune_ternary_width op width arg1 arg2 arg3 =
      TernaryWidth (op, width, prune arg1 minus_one, prune arg2 minus_one, prune arg3 minus_one)
    in match expr with
      IntConst (IntLiteral int) -> IntConst (IntLiteral (logand int needed))
    | IntConst _ | FloatConst _ | ConditionConst _ | Register _ | LoadBO _ -> expr
    | Unary (op, arg) -> prune_unary op arg
    | UnaryWidth (op, width, arg) -> prune_unary_width op width arg
    | Binary (op, arg1, arg2) -> prune_binary op arg1 arg2
    | BinaryWidth (op, width, arg1, arg2) -> prune_binary_width op width arg1 arg2
    | TernaryWidth (op, width, arg1, arg2, arg3) -> prune_ternary_width op width arg1 arg2 arg3
    | Extract (arg, start, length) ->
	(match (start, length) with
	  (IntLiteral start_int, IntLiteral length_int) ->
	    let parg = prune arg (logand (shiftl needed start_int) (bitmask start_int length_int))
	    and upper_mask = shiftl minus_one (add start_int length_int)
	    in if ((logxor upper_mask (known arg)) = 0L
                   && (logand upper_mask (bits arg)) = 0L) then
		BinaryWidth (LShiftR, 8, parg, IntConst start)
	      else
		Extract (parg, start, length)
	| (IntLiteral start_int, _) ->
	    let parg = prune arg (shiftl needed start_int)
	    in Extract (parg, start, length)
	| _ -> Extract ((prune arg minus_one), start, length))
    | Insert (arg1, arg2, start, length) ->
	(match (start, length) with
	  (IntLiteral start_int, IntLiteral length_int) ->
	    let parg1 = prune arg1 (insert_bits needed zero start_int length_int)
	    and parg2 = prune arg2 (extract_bits needed start_int length_int)
	    in if (extract_bits needed start_int length_int) = 0L then
	      parg1
	    else if (logand needed (lognot (bitmask start_int length_int))) = zero then
	      Binary (ShiftL, parg2, IntConst (IntLiteral start_int))
	    else
	      Insert (parg1, parg2, start, length)
	| (IntLiteral start_int, _) ->
	    let parg1 = prune arg1 needed
	    and parg2 = prune arg2 (lshiftr needed start_int)
	    in if (lshiftr needed start_int) = 0L then
	      parg1
	    else
	      Insert (parg1, parg2, start, length)
	| _ ->
	    Insert (prune arg1 needed, prune arg2 minus_one, start, length))
    | If (condition, cons, alt) ->
	If (prune condition one, prune cons needed, prune alt needed)

let prune_stmt stmt =
  match stmt with
      Store (byte_order, width, addr, value) ->
	Store (byte_order, width, prune addr (width_mask (machine_addr_width ())), prune value (width_mask width))
    | Assign (register, value) ->
	Assign (register, prune value (width_mask (machine_register_width register)))
