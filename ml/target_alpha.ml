open List
open Int64

open Expr
open Matcher

let lda_insn =
  { pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = fun f s b ->
      let i = get_int_const_binding f b "i"
      in if ((shift_right i 15) = 0L) || ((shift_right i 15) = minus_one) then Some 1 else None }

let ldah_insn =
  { pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = fun f s b ->
      let i = get_int_const_binding f b "i"
      in if ((logand i 0xffffL) = 0L) && (((shift_right i 31) = 0L) || ((shift_right i 31) = minus_one)) then Some 1 else None }

let load_insn =
  { pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = fun f s b -> Some 2 }

let mov_insn =
  { pattern = AssignPattern ("rs", RegisterPattern "ra") ;
    matcher = fun f s b -> Some 1 }

let and_insn =
  { pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = fun f s b -> Some 1 }

let bis_insn =
  { pattern = AssignPattern ("rs", BinaryPattern (BitOr, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = fun f s b -> Some 1 }

let bic_insn =
  { pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", UnaryPattern (BitNeg, RegisterPattern "rb"))) ;
    matcher = fun f s b -> Some 1 }

let neg_insn =
  { pattern = AssignPattern ("rs", UnaryPattern (BitNeg, RegisterPattern "rb")) ;
    matcher = fun f s b -> Some 1 }

let sll_imm_insn =
  { pattern = AssignPattern ("rs", BinaryPattern (ShiftL, RegisterPattern "ra", IntPattern (AnyInt "a"))) ;
    matcher = fun f s b -> Some 1 }

let zapnot_imm_srl_imm_insn =
  { pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([1; 2; 4], "width"),
						       RegisterPattern "ra", IntPattern (AnyInt "a"))) ;
    matcher = fun f s b -> Some 2 }

let srl_imm_insn =
  { pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([8], "width"),
						       RegisterPattern "ra", IntPattern (AnyInt "a"))) ;
    matcher = fun f s b -> Some 1 }

let alpha_insns =
  [ lda_insn ; ldah_insn ; load_insn ;
    mov_insn ; and_insn ; bis_insn ; bic_insn ; neg_insn ;
    sll_imm_insn; zapnot_imm_srl_imm_insn; srl_imm_insn ]
