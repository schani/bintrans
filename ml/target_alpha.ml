open List
open Int64

open Expr
open Cond_monad
open Matcher

let lda_insn =
  { name = "lda" ;
    pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = fun f s b ->
      let i = get_const_binding f b "i"
      in cm_when f (or_expr (is_zero_expr (BinaryWidth (AShiftR, 8, i, int_literal_expr 15L)))
		      (is_full_mask_expr (BinaryWidth (AShiftR, 8, i, int_literal_expr 15L))))
	  (fun _ -> cm_return 1) }

let ldah_insn =
  { name = "ldah" ;
    pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = fun f s b ->
      let i = get_const_binding f b "i"
      in cm_when f (and_expr (is_zero_expr (bitand_expr i (int_literal_expr 0xffffL)))
		      (or_expr (is_zero_expr (BinaryWidth (AShiftR, 8, i, int_literal_expr 31L)))
			 (is_full_mask_expr (BinaryWidth (AShiftR, 8, i, int_literal_expr 31L)))))
	  (fun _ -> cm_return 1) }

let load_insn =
  { name = "load_int" ;
    pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = fun f s b -> cm_return 2 }

let mov_insn =
  { name = "mov" ;
    pattern = AssignPattern ("rs", RegisterPattern "ra") ;
    matcher = fun f s b -> cm_return 1 }

let and_insn =
  { name = "and" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = fun f s b -> cm_return 1 }

let bis_insn =
  { name = "bis" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitOr, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = fun f s b -> cm_return 1 }

let bic_insn =
  { name = "bic" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", UnaryPattern (BitNeg, RegisterPattern "rb"))) ;
    matcher = fun f s b -> cm_return 1 }

let neg_insn =
  { name = "neg" ;
    pattern = AssignPattern ("rs", UnaryPattern (BitNeg, RegisterPattern "rb")) ;
    matcher = fun f s b -> cm_return 1 }

let sll_imm_insn =
  { name = "sll_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (ShiftL, RegisterPattern "ra", IntPattern (AnyInt "a"))) ;
    matcher = fun f s b -> cm_return 1 }

let zapnot_imm_srl_imm_insn =
  { name = "zapnot_imm->srl_imm" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([1; 2; 4], "width"),
						       RegisterPattern "ra", IntPattern (AnyInt "a"))) ;
    matcher = fun f s b -> cm_return 2 }

let srl_imm_insn =
  { name = "srl_imm" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([8], "width"),
						       RegisterPattern "ra", IntPattern (AnyInt "a"))) ;
    matcher = fun f s b -> cm_return 1 }

let alpha_insns =
  [ lda_insn ; ldah_insn ; load_insn ;
    mov_insn ; and_insn ; bis_insn ; bic_insn ; neg_insn ;
    sll_imm_insn; zapnot_imm_srl_imm_insn; srl_imm_insn ]
