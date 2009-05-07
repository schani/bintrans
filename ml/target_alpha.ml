open Int64

open Expr
open Uncertainty
open Matcher
open Irmacros
open Cgen

let addq_matcher =
  { name = "addq" ;
    pattern = AssignPattern ("rs", BinaryPattern (IntAdd, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and addq_printer =
  ("addq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* addq */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " + " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and addq_gen_gen =
  ("addq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_ADDQ(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let addl_matcher =
  { name = "addl" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "ra", RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and addl_printer =
  ("addl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* addl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_32(" ^ (register_to_c alloc ra) ^ " + " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and addl_gen_gen =
  ("addl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_ADDL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let addl_imm_matcher =
  { name = "addl_imm" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and addl_imm_printer =
  ("addl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* addl_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_32(" ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc i) ^ ");" ^ " }"))
and addl_imm_gen_gen =
  ("addl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_ADDL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let addl_31_matcher =
  { name = "addl_31" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), RegisterPattern "ra")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and addl_31_printer =
  ("addl_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "/* addl_31 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_32(" ^ (register_to_c alloc ra) ^ ");" ^ " }"))
and addl_31_gen_gen =
  ("addl_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_ADDL(" ^ (register_to_c_gen alloc ra) ^ ", 31, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let eqv_matcher =
  { name = "eqv" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitXor, RegisterPattern "ra", UnaryPattern (BitNeg, RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and eqv_printer =
  ("eqv", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* eqv */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " ^ " ^ "~" ^ "(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and eqv_gen_gen =
  ("eqv", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_EQV(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let eqv_imm_matcher =
  { name = "eqv_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitXor, RegisterPattern "ra", UnaryPattern (BitNeg, IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and eqv_imm_printer =
  ("eqv_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* eqv_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " ^ " ^ "~" ^ "(" ^ (expr_to_c alloc i) ^ ");" ^ " }"))
and eqv_imm_gen_gen =
  ("eqv_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_EQV_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let lda_matcher =
  { name = "lda" ;
    pattern = AssignPattern ("rs", BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (15L)))) (fun _ -> uc_return 1)) }
and lda_printer =
  ("lda", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* lda */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (15L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and lda_gen_gen =
  ("lda", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (15L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDA(" ^ (register_to_c_gen alloc rs) ^ ", " ^ (expr_to_c alloc i) ^ " & 0xffff, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let lda_31_matcher =
  { name = "lda_31" ;
    pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (15L)))) (fun _ -> uc_return 1)) }
and lda_31_printer =
  ("lda_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and i = get_const_binding bindings "i"
              in "/* lda_31 */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (15L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and lda_31_gen_gen =
  ("lda_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (15L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDA(" ^ (register_to_c_gen alloc rs) ^ ", " ^ (expr_to_c alloc i) ^ " & 0xffff, 31));" ^ " }"))

let ldah_matcher =
  { name = "ldah" ;
    pattern = AssignPattern ("rs", BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (65535L))), make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (31L))))) (fun _ -> uc_return 1)) }
and ldah_printer =
  ("ldah", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* ldah */ { assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (65535L))), make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (31L))))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and ldah_gen_gen =
  ("ldah", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (65535L))), make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (31L))))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDAH(" ^ (register_to_c_gen alloc rs) ^ ", (" ^ (expr_to_c alloc i) ^ " >> 16) & 0xffff, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let ldah_31_matcher =
  { name = "ldah_31" ;
    pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (65535L))), make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (31L))))) (fun _ -> uc_return 1)) }
and ldah_31_printer =
  ("ldah_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and i = get_const_binding bindings "i"
              in "/* ldah_31 */ { assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (65535L))), make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (31L))))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and ldah_31_gen_gen =
  ("ldah_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (65535L))), make_zero_or_full_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (31L))))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDAH(" ^ (register_to_c_gen alloc rs) ^ ", (" ^ (expr_to_c alloc i) ^ " >> 16) & 0xffff, 31));" ^ " }"))

let load_int_matcher =
  { name = "load_int" ;
    pattern = AssignPattern ("rs", IntPattern (AnyInt "i")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_return 2) }
and load_int_printer =
  ("load_int", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and i = get_const_binding bindings "i"
              in "/* load_int */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and load_int_gen_gen =
  ("load_int", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit_load_integer_64(" ^ (register_to_c_gen alloc rs) ^ ", " ^ (expr_to_c alloc i) ^ ");" ^ " }"))

let mov_matcher =
  { name = "mov" ;
    pattern = AssignPattern ("rs", RegisterPattern "ra") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and mov_printer =
  ("mov", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "/* mov */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ ";" ^ " }"))
and mov_gen_gen =
  ("mov", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_MOV(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let and_matcher =
  { name = "and" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and and_printer =
  ("and", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* and */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " & " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and and_gen_gen =
  ("and", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_AND(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let and_imm_matcher =
  { name = "and_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and and_imm_printer =
  ("and_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* and_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " & " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and and_imm_gen_gen =
  ("and_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_AND_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let bic_imm_for_and_matcher =
  { name = "bic_imm_for_and" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_full_mask_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and bic_imm_for_and_printer =
  ("bic_imm_for_and", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* bic_imm_for_and */ { assert(" ^ (expr_to_c [] (and_expr (make_full_mask_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " & " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and bic_imm_for_and_gen_gen =
  ("bic_imm_for_and", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_full_mask_p (BinaryWidth (AShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_BIC_IMM(" ^ (register_to_c_gen alloc ra) ^ ", unary_BitNeg(" ^ (expr_to_c alloc i) ^ "), " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let zapnot_imm_matcher =
  { name = "zapnot_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (UserOp ("IsMaskMask", [i ; int_literal_expr (8L)])) (fun _ -> uc_return 1)) }
and zapnot_imm_printer =
  ("zapnot_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* zapnot_imm */ { assert(" ^ (expr_to_c [] (and_expr (UserOp ("IsMaskMask", [i ; int_literal_expr (8L)])) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " & " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and zapnot_imm_gen_gen =
  ("zapnot_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (UserOp ("IsMaskMask", [i ; int_literal_expr (8L)])) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_ZAPNOT_IMM(" ^ (register_to_c_gen alloc ra) ^ ", reverse_maskmask(" ^ (expr_to_c alloc i) ^ ", 8), " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let bis_matcher =
  { name = "bis" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitOr, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and bis_printer =
  ("bis", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* bis */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " | " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and bis_gen_gen =
  ("bis", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_BIS(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let bis_imm_matcher =
  { name = "bis_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitOr, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and bis_imm_printer =
  ("bis_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* bis_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " | " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and bis_imm_gen_gen =
  ("bis_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_BIS_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let bic_matcher =
  { name = "bic" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", UnaryPattern (BitNeg, RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and bic_printer =
  ("bic", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* bic */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " | " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and bic_gen_gen =
  ("bic", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_BIC(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let bic_imm_matcher =
  { name = "bic_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, RegisterPattern "ra", UnaryPattern (BitNeg, IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and bic_imm_printer =
  ("bic_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* bic_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " | " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and bic_imm_gen_gen =
  ("bic_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_BIC_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let mull_matcher =
  { name = "mull" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), BinaryPattern (IntMul, RegisterPattern "ra", RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and mull_printer =
  ("mull", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* mull */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_32(" ^ (register_to_c alloc ra) ^ " * " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and mull_gen_gen =
  ("mull", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_MULL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let mull_imm_matcher =
  { name = "mull_imm" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), BinaryPattern (IntMul, RegisterPattern "ra", IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and mull_imm_printer =
  ("mull_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* mull_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_32(" ^ (register_to_c alloc ra) ^ " * " ^ (expr_to_c alloc i) ^ ");" ^ " }"))
and mull_imm_gen_gen =
  ("mull_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_MULL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let mulq_matcher =
  { name = "mulq" ;
    pattern = AssignPattern ("rs", BinaryPattern (IntMul, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and mulq_printer =
  ("mulq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* mulq */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " * " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and mulq_gen_gen =
  ("mulq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_MULQ(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let mulq_imm_matcher =
  { name = "mulq_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (IntMul, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and mulq_imm_printer =
  ("mulq_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* mulq_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " * " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and mulq_imm_gen_gen =
  ("mulq_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_MULQ_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let neg_matcher =
  { name = "neg" ;
    pattern = AssignPattern ("rs", UnaryPattern (BitNeg, RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and neg_printer =
  ("neg", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* neg */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ "~" ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and neg_gen_gen =
  ("neg", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_NOT(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let negl_matcher =
  { name = "negl" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), UnaryPattern (IntNeg, RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and negl_printer =
  ("negl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* negl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_32(" ^ "~" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and negl_gen_gen =
  ("negl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_NEGL(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let negq_matcher =
  { name = "negq" ;
    pattern = AssignPattern ("rs", UnaryPattern (IntNeg, RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and negq_printer =
  ("negq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* negq */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ "~" ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and negq_gen_gen =
  ("negq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_NEGQ(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let ornot_matcher =
  { name = "ornot" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitOr, RegisterPattern "ra", UnaryPattern (BitNeg, RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and ornot_printer =
  ("ornot", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* ornot */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " | " ^ "~" ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and ornot_gen_gen =
  ("ornot", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_ORNOT(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let sll_matcher =
  { name = "sll" ;
    pattern = AssignPattern ("rs", BinaryPattern (ShiftL, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and sll_printer =
  ("sll", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* sll */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " << " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and sll_gen_gen =
  ("sll", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SLL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let sll_imm_matcher =
  { name = "sll_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (ShiftL, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and sll_imm_printer =
  ("sll_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* sll_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " << " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and sll_imm_gen_gen =
  ("sll_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_SLL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let sra_matcher =
  { name = "sra" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (AShiftR, ([8], "__dummy__"), RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and sra_printer =
  ("sra", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* sra */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = ashiftr_64(" ^ (register_to_c alloc ra) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and sra_gen_gen =
  ("sra", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SRA(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let sra_imm_matcher =
  { name = "sra_imm" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (AShiftR, ([8], "__dummy__"), RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and sra_imm_printer =
  ("sra_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* sra_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = ashiftr_64(" ^ (register_to_c alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ");" ^ " }"))
and sra_imm_gen_gen =
  ("sra_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_SRA_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let srl_matcher =
  { name = "srl" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([8], "__dummy__"), RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and srl_printer =
  ("srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* srl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " >> " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and srl_gen_gen =
  ("srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SRL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let srl_imm_matcher =
  { name = "srl_imm" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([8], "__dummy__"), RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and srl_imm_printer =
  ("srl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* srl_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " >> " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and srl_imm_gen_gen =
  ("srl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_SRL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let zapnot_imm_srl_matcher =
  { name = "zapnot_imm_srl" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([1; 2; 4], "width"), RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and width = get_width_binding bindings "width"
                 in uc_return 2) }
and zapnot_imm_srl_printer =
  ("zapnot_imm_srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and width = get_width_binding bindings "width" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* zapnot_imm_srl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " & width_mask(" ^ (string_of_int width) ^ ")) >> " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and zapnot_imm_srl_gen_gen =
  ("zapnot_imm_srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and width = get_width_binding bindings "width" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(compose_width_zapnot(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (string_of_int width) ^ ", " ^ (register_to_c_gen alloc rs) ^ ")); emit(COMPOSE_SRL(" ^ (register_to_c_gen alloc rs) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let zapnot_imm_srl_imm_matcher =
  { name = "zapnot_imm_srl_imm" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([1; 2; 4], "width"), RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and width = get_width_binding bindings "width" and i = get_const_binding bindings "i"
                 in uc_return 2) }
and zapnot_imm_srl_imm_printer =
  ("zapnot_imm_srl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and width = get_width_binding bindings "width" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* zapnot_imm_srl_imm */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " & width_mask(" ^ (string_of_int width) ^ ")) >> " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and zapnot_imm_srl_imm_gen_gen =
  ("zapnot_imm_srl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and width = get_width_binding bindings "width" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(compose_width_zapnot(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (string_of_int width) ^ ", " ^ (register_to_c_gen alloc rs) ^ ")); emit(COMPOSE_SRL_IMM(" ^ (register_to_c_gen alloc rs) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let srl_matcher =
  { name = "srl" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([8], "__dummy__"), RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and srl_printer =
  ("srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* srl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " >> " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and srl_gen_gen =
  ("srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SRL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let srl_imm_matcher =
  { name = "srl_imm" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([8], "__dummy__"), RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and srl_imm_printer =
  ("srl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* srl_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " >> " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and srl_imm_gen_gen =
  ("srl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_SRL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extbl_imm_8_matcher =
  { name = "extbl_imm_8" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, BinaryWidthPattern (LShiftR, ([8], "__dummy__"), RegisterPattern "ra", IntPattern (AnyInt "i")), IntPattern (TheInt (int_literal_expr (255L))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (6L))))) (fun _ -> uc_return 1)) }
and extbl_imm_8_printer =
  ("extbl_imm_8", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* extbl_imm_8 */ { assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (6L))))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " >> " ^ (expr_to_c alloc i) ^ ") & 0xff;" ^ " }"))
and extbl_imm_8_gen_gen =
  ("extbl_imm_8", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (6L))))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_EXTBL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ " >> 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extbl_imm_4_matcher =
  { name = "extbl_imm_4" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, BinaryWidthPattern (LShiftR, ([4], "__dummy__"), RegisterPattern "ra", IntPattern (AnyInt "i")), IntPattern (TheInt (int_literal_expr (255L))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (5L))))) (fun _ -> uc_return 1)) }
and extbl_imm_4_printer =
  ("extbl_imm_4", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* extbl_imm_4 */ { assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (5L))))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " >> " ^ (expr_to_c alloc i) ^ ") & 0xff;" ^ " }"))
and extbl_imm_4_gen_gen =
  ("extbl_imm_4", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (5L))))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_EXTBL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ " >> 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extbl_imm_4_3_matcher =
  { name = "extbl_imm_4_3" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([4], "__dummy__"), RegisterPattern "ra", IntPattern (TheInt (int_literal_expr (24L))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and extbl_imm_4_3_printer =
  ("extbl_imm_4_3", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "/* extbl_imm_4_3 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " >> 24) & 0xff;" ^ " }"))
and extbl_imm_4_3_gen_gen =
  ("extbl_imm_4_3", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_EXTBL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extwl_imm_8_matcher =
  { name = "extwl_imm_8" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, BinaryWidthPattern (LShiftR, ([8], "__dummy__"), RegisterPattern "ra", IntPattern (AnyInt "i")), IntPattern (TheInt (int_literal_expr (65535L))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (6L))))) (fun _ -> uc_return 1)) }
and extwl_imm_8_printer =
  ("extwl_imm_8", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* extwl_imm_8 */ { assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (6L))))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " >> " ^ (expr_to_c alloc i) ^ ") & 0xffff;" ^ " }"))
and extwl_imm_8_gen_gen =
  ("extwl_imm_8", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (6L))))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_EXTWL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ " >> 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extwl_imm_4_matcher =
  { name = "extwl_imm_4" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitAnd, BinaryWidthPattern (LShiftR, ([4], "__dummy__"), RegisterPattern "ra", IntPattern (AnyInt "i")), IntPattern (TheInt (int_literal_expr (65535L))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (5L))))) (fun _ -> uc_return 1)) }
and extwl_imm_4_printer =
  ("extwl_imm_4", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* extwl_imm_4 */ { assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (5L))))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " >> " ^ (expr_to_c alloc i) ^ ") & 0xffff;" ^ " }"))
and extwl_imm_4_gen_gen =
  ("extwl_imm_4", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (ConditionAnd, make_int_zero_p (Binary (BitAnd, i, int_literal_expr (7L))), make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (5L))))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_EXTWL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ " >> 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extwl_imm_4_2_matcher =
  { name = "extwl_imm_4_2" ;
    pattern = AssignPattern ("rs", BinaryWidthPattern (LShiftR, ([4], "__dummy__"), RegisterPattern "ra", IntPattern (TheInt (int_literal_expr (16L))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and extwl_imm_4_2_printer =
  ("extwl_imm_4_2", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "/* extwl_imm_4_2 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " >> 16) & 0xffff;" ^ " }"))
and extwl_imm_4_2_gen_gen =
  ("extwl_imm_4_2", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_EXTWL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", 2, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let sextb_matcher =
  { name = "sextb" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([1], "__dummy__"), RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and sextb_printer =
  ("sextb", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* sextb */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_8(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and sextb_gen_gen =
  ("sextb", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SEXTB(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let sextw_matcher =
  { name = "sextw" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([2], "__dummy__"), RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and sextw_printer =
  ("sextw", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* sextw */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_16(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and sextw_gen_gen =
  ("sextw", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SEXTW(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let subl_matcher =
  { name = "subl" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), make__i_pattern (RegisterPattern "ra") (RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and subl_printer =
  ("subl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* subl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc ra) ^ " = sex_32(" ^ (register_to_c alloc ra) ^ " - " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and subl_gen_gen =
  ("subl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SUBL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let subl_imm_matcher =
  { name = "subl_imm" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Sex, ([4], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, Unary (IntNeg, i), int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and subl_imm_printer =
  ("subl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* subl_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, Unary (IntNeg, i), int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = sex_32(" ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc i) ^ ");" ^ " }"))
and subl_imm_gen_gen =
  ("subl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, Unary (IntNeg, i), int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_SUBL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", -" ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let subq_matcher =
  { name = "subq" ;
    pattern = AssignPattern ("rs", make__i_pattern (RegisterPattern "ra") (RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and subq_printer =
  ("subq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* subq */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " - " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and subq_gen_gen =
  ("subq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_SUBQ(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let subq_imm_matcher =
  { name = "subq_imm" ;
    pattern = AssignPattern ("rs", make__i_pattern (RegisterPattern "ra") (IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and subq_imm_printer =
  ("subq_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* subq_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " - " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and subq_imm_gen_gen =
  ("subq_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_SUBQ_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let xor_matcher =
  { name = "xor" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitXor, RegisterPattern "ra", RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and xor_printer =
  ("xor", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* xor */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " ^ " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and xor_gen_gen =
  ("xor", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_XOR(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let xor_imm_matcher =
  { name = "xor_imm" ;
    pattern = AssignPattern ("rs", BinaryPattern (BitXor, RegisterPattern "ra", IntPattern (AnyInt "i"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and xor_imm_printer =
  ("xor_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* xor_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " ^ " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and xor_imm_gen_gen =
  ("xor_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_XOR_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let zapnot_imm_1_matcher =
  { name = "zapnot_imm_1" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Zex, ([1], "__dummy__"), RegisterPattern "ra")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and zapnot_imm_1_printer =
  ("zapnot_imm_1", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "/* zapnot_imm_1 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = zex_8(" ^ (register_to_c alloc ra) ^ ");" ^ " }"))
and zapnot_imm_1_gen_gen =
  ("zapnot_imm_1", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_ZAPNOT_IMM(" ^ (register_to_c_gen alloc ra) ^ ", 1, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let zapnot_imm_3_matcher =
  { name = "zapnot_imm_3" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Zex, ([2], "__dummy__"), RegisterPattern "ra")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and zapnot_imm_3_printer =
  ("zapnot_imm_3", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "/* zapnot_imm_3 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = zex_16(" ^ (register_to_c alloc ra) ^ ");" ^ " }"))
and zapnot_imm_3_gen_gen =
  ("zapnot_imm_3", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_ZAPNOT_IMM(" ^ (register_to_c_gen alloc ra) ^ ", 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let zapnot_imm_15_matcher =
  { name = "zapnot_imm_15" ;
    pattern = AssignPattern ("rs", UnaryWidthPattern (Zex, ([4], "__dummy__"), RegisterPattern "ra")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and zapnot_imm_15_printer =
  ("zapnot_imm_15", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "/* zapnot_imm_15 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = zex_32(" ^ (register_to_c alloc ra) ^ ");" ^ " }"))
and zapnot_imm_15_gen_gen =
  ("zapnot_imm_15", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_ZAPNOT_IMM(" ^ (register_to_c_gen alloc ra) ^ ", 15, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let zapnot_imm_sll_srl_imm_bis_matcher =
  { name = "zapnot_imm_sll_srl_imm_bis" ;
    pattern = AssignPattern ("rs", make_rotl_pattern 4 (RegisterPattern "ra") (RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 4) }
and zapnot_imm_sll_srl_imm_bis_printer =
  ("zapnot_imm_sll_srl_imm_bis", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* zapnot_imm_sll_srl_imm_bis */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "{ word_64 tmp = (" ^ (register_to_c alloc ra) ^ " & 0xffffffff) << " ^ (register_to_c alloc rb) ^ "; " ^ (register_to_c alloc rs) ^ " = tmp | (tmp >> 32); }" ^ " }"))
and zapnot_imm_sll_srl_imm_bis_gen_gen =
  ("zapnot_imm_sll_srl_imm_bis", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ 
"
{ reg_t tmp;
      emit(COMPOSE_ZAPNOT_IMM(" ^ 
(register_to_c_gen alloc ra) ^ 
", 15, " ^ 
(register_to_c_gen alloc rs) ^ 
"
));
      emit(COMPOSE_SLL(" ^ 
(register_to_c_gen alloc rs) ^ 
", " ^ 
(register_to_c_gen alloc rb) ^ 
", " ^ 
(register_to_c_gen alloc rs) ^ 
"
));
      tmp = alloc_tmp_integer_reg();
      emit(COMPOSE_SRL_IMM(" ^ 
(register_to_c_gen alloc rs) ^ 
"
, 32, tmp));
      emit(COMPOSE_BIS(" ^ 
(register_to_c_gen alloc rs) ^ 
", tmp, " ^ 
(register_to_c_gen alloc rs) ^ 
"
));
      free_tmp_integer_reg(tmp); }" ^ " }"))

let extract_and_imm_matcher =
  { name = "extract_and_imm" ;
    pattern = AssignPattern ("rs", ExtractPattern (RegisterPattern "ra", TheInt (int_literal_expr (0L)), AnyInt "l")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and l = get_const_binding bindings "l"
                 in uc_when fields (Binary (LessU, l, int_literal_expr (8L))) (fun _ -> uc_return 1)) }
and extract_and_imm_printer =
  ("extract_and_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and l = get_const_binding bindings "l"
              in "/* extract_and_imm */ { assert(" ^ (expr_to_c [] (and_expr (Binary (LessU, l, int_literal_expr (8L))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = bit_extract(" ^ (register_to_c alloc ra) ^ ", 0, " ^ (expr_to_c alloc l) ^ ");" ^ " }"))
and extract_and_imm_gen_gen =
  ("extract_and_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and l = get_const_binding bindings "l"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (LessU, l, int_literal_expr (8L))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_AND_IMM(" ^ (register_to_c_gen alloc ra) ^ ", (1 << " ^ (expr_to_c alloc l) ^ ") - 1, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extract_zapnot_imm_matcher =
  { name = "extract_zapnot_imm" ;
    pattern = AssignPattern ("rs", ExtractPattern (RegisterPattern "ra", TheInt (int_literal_expr (0L)), AnyInt "l")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and l = get_const_binding bindings "l"
                 in uc_when fields (make_int_zero_p (Binary (BitAnd, l, int_literal_expr (7L)))) (fun _ -> uc_return 1)) }
and extract_zapnot_imm_printer =
  ("extract_zapnot_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and l = get_const_binding bindings "l"
              in "/* extract_zapnot_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (Binary (BitAnd, l, int_literal_expr (7L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = bit_extract(" ^ (register_to_c alloc ra) ^ ", 0, " ^ (expr_to_c alloc l) ^ ");" ^ " }"))
and extract_zapnot_imm_gen_gen =
  ("extract_zapnot_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and l = get_const_binding bindings "l"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (Binary (BitAnd, l, int_literal_expr (7L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_ZAPNOT_IMM(" ^ (register_to_c_gen alloc ra) ^ ", (1 << (" ^ (expr_to_c alloc l) ^ " >> 3)) - 1, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extract_srl_matcher =
  { name = "extract_srl" ;
    pattern = AssignPattern ("rs", ExtractPattern (RegisterPattern "ra", AnyInt "s", AnyInt "l")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
                 in uc_when fields (Binary (IntEqual, Binary (IntAdd, s, l), int_literal_expr (64L))) (fun _ -> uc_return 1)) }
and extract_srl_printer =
  ("extract_srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
              in "/* extract_srl */ { assert(" ^ (expr_to_c [] (and_expr (Binary (IntEqual, Binary (IntAdd, s, l), int_literal_expr (64L))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = bit_extract(" ^ (register_to_c alloc ra) ^ ", " ^ (expr_to_c alloc s) ^ ", " ^ (expr_to_c alloc l) ^ ");" ^ " }"))
and extract_srl_gen_gen =
  ("extract_srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (Binary (IntEqual, Binary (IntAdd, s, l), int_literal_expr (64L))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_SRL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", 64 - " ^ (expr_to_c alloc s) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extract_extbl_imm_matcher =
  { name = "extract_extbl_imm" ;
    pattern = AssignPattern ("rs", ExtractPattern (RegisterPattern "ra", AnyInt "s", TheInt (int_literal_expr (8L)))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and s = get_const_binding bindings "s"
                 in uc_when fields (make_int_zero_p (Binary (BitAnd, s, int_literal_expr (7L)))) (fun _ -> uc_return 1)) }
and extract_extbl_imm_printer =
  ("extract_extbl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s"
              in "/* extract_extbl_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (Binary (BitAnd, s, int_literal_expr (7L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = bit_extract(" ^ (register_to_c alloc ra) ^ ", " ^ (expr_to_c alloc s) ^ ", 8);" ^ " }"))
and extract_extbl_imm_gen_gen =
  ("extract_extbl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (Binary (BitAnd, s, int_literal_expr (7L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_EXTBL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc s) ^ " >> 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extract_extwl_imm_matcher =
  { name = "extract_extwl_imm" ;
    pattern = AssignPattern ("rs", ExtractPattern (RegisterPattern "ra", AnyInt "s", TheInt (int_literal_expr (16L)))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and s = get_const_binding bindings "s"
                 in uc_when fields (make_int_zero_p (Binary (BitAnd, s, int_literal_expr (7L)))) (fun _ -> uc_return 1)) }
and extract_extwl_imm_printer =
  ("extract_extwl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s"
              in "/* extract_extwl_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (Binary (BitAnd, s, int_literal_expr (7L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = bit_extract(" ^ (register_to_c alloc ra) ^ ", " ^ (expr_to_c alloc s) ^ ", 16);" ^ " }"))
and extract_extwl_imm_gen_gen =
  ("extract_extwl_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (Binary (BitAnd, s, int_literal_expr (7L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_EXTWL_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc s) ^ " >> 3, " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let extract_sll_srl_matcher =
  { name = "extract_sll_srl" ;
    pattern = AssignPattern ("rs", ExtractPattern (RegisterPattern "ra", AnyInt "s", AnyInt "l")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
                 in uc_return 2) }
and extract_sll_srl_printer =
  ("extract_sll_srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
              in "/* extract_sll_srl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = bit_extract(" ^ (register_to_c alloc ra) ^ ", " ^ (expr_to_c alloc s) ^ ", " ^ (expr_to_c alloc l) ^ ");" ^ " }"))
and extract_sll_srl_gen_gen =
  ("extract_sll_srl", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ 
"
{ reg_t tmp = alloc_tmp_integer_reg();
      emit(COMPOSE_SLL_IMM(" ^ 
(register_to_c_gen alloc ra) ^ 
", 64 - " ^ 
(expr_to_c alloc s) ^ 
" - " ^ 
(expr_to_c alloc l) ^ 
"
, tmp));
      emit(COMPOSE_SRL_IMM(tmp, 64 - " ^ 
(expr_to_c alloc l) ^ 
", " ^ 
(register_to_c_gen alloc rs) ^ 
"
));
      free_tmp_integer_reg(tmp); }" ^ " }"))

let insert_full_matcher =
  { name = "insert_full" ;
    pattern = AssignPattern ("rs", InsertPattern (RegisterPattern "ra", RegisterPattern "rb", AnyInt "s", AnyInt "l")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
                 in uc_return 7) }
and insert_full_printer =
  ("insert_full", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
              in "/* insert_full */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = bit_insert(" ^ (register_to_c alloc ra) ^ ", " ^ (register_to_c alloc rb) ^ ", " ^ (expr_to_c alloc s) ^ ", " ^ (expr_to_c alloc l) ^ ");" ^ " }"))
and insert_full_gen_gen =
  ("insert_full", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and s = get_const_binding bindings "s" and l = get_const_binding bindings "l"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ 
"
{ reg_t tmp = alloc_tmp_integer_reg();
      bt_assert(" ^ 
(register_to_c_gen alloc rb) ^ 
" != " ^ 
(register_to_c_gen alloc rs) ^ 
"
);
      emit(COMPOSE_NOT(31, tmp));
      emit(COMPOSE_SLL_IMM(tmp, 64 - " ^ 
(expr_to_c alloc l) ^ 
"
, tmp));
      emit(COMPOSE_SRL_IMM(tmp, 64 - (" ^ 
(expr_to_c alloc s) ^ 
" + " ^ 
(expr_to_c alloc l) ^ 
"
), tmp));
      emit(COMPOSE_BIC(" ^ 
(register_to_c_gen alloc ra) ^ 
", tmp, " ^ 
(register_to_c_gen alloc rs) ^ 
"
));
      emit(COMPOSE_SLL_IMM(" ^ 
(register_to_c_gen alloc rb) ^ 
", 64 - " ^ 
(expr_to_c alloc l) ^ 
"
, tmp));
      emit(COMPOSE_SRL_IMM(tmp, 64 - (" ^ 
(expr_to_c alloc s) ^ 
" + " ^ 
(expr_to_c alloc l) ^ 
"
), tmp));
      emit(COMPOSE_BIS(tmp, " ^ 
(register_to_c_gen alloc rs) ^ 
", " ^ 
(register_to_c_gen alloc rs) ^ 
"
));
      free_tmp_integer_reg(tmp); }" ^ " }"))

let ldbu_matcher =
  { name = "ldbu" ;
    pattern = AssignPattern ("ra", UnaryPattern (LoadByte, RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and ldbu_printer =
  ("ldbu", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* ldbu */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_8(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldbu_gen_gen =
  ("ldbu", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_LDBU(" ^ (register_to_c_gen alloc ra) ^ ", 0, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldbu_disp_matcher =
  { name = "ldbu_disp" ;
    pattern = AssignPattern ("ra", UnaryPattern (LoadByte, BinaryPattern (IntAdd, RegisterPattern "rb", IntPattern (AnyInt "disp")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and ldbu_disp_printer =
  ("ldbu_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "/* ldbu_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_8(" ^ (expr_to_c alloc disp) ^ " + " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldbu_disp_gen_gen =
  ("ldbu_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDBU(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldbu_zex_matcher =
  { name = "ldbu_zex" ;
    pattern = AssignPattern ("ra", UnaryWidthPattern (Zex, ([1], "__dummy__"), UnaryPattern (LoadByte, RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and ldbu_zex_printer =
  ("ldbu_zex", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* ldbu_zex */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_8(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldbu_zex_gen_gen =
  ("ldbu_zex", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_LDBU(" ^ (register_to_c_gen alloc ra) ^ ", 0, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldbu_zex_disp_matcher =
  { name = "ldbu_zex_disp" ;
    pattern = AssignPattern ("ra", UnaryWidthPattern (Zex, ([1], "__dummy__"), UnaryPattern (LoadByte, BinaryPattern (IntAdd, RegisterPattern "rb", IntPattern (AnyInt "disp"))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and ldbu_zex_disp_printer =
  ("ldbu_zex_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "/* ldbu_zex_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_8(" ^ (expr_to_c alloc disp) ^ " + " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldbu_zex_disp_gen_gen =
  ("ldbu_zex_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDBU(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldwu_matcher =
  { name = "ldwu" ;
    pattern = AssignPattern ("ra", LoadBOPattern (LittleEndian, ([2], "__dummy__"), RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and ldwu_printer =
  ("ldwu", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* ldwu */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_16(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldwu_gen_gen =
  ("ldwu", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_LDWU(" ^ (register_to_c_gen alloc ra) ^ ", 0, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldwu_disp_matcher =
  { name = "ldwu_disp" ;
    pattern = AssignPattern ("ra", LoadBOPattern (LittleEndian, ([2], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "rb", IntPattern (AnyInt "disp")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and ldwu_disp_printer =
  ("ldwu_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "/* ldwu_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_16(" ^ (expr_to_c alloc disp) ^ " + " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldwu_disp_gen_gen =
  ("ldwu_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDWU(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldwu_zex_matcher =
  { name = "ldwu_zex" ;
    pattern = AssignPattern ("ra", UnaryWidthPattern (Zex, ([2], "__dummy__"), LoadBOPattern (LittleEndian, ([2], "__dummy__"), RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and ldwu_zex_printer =
  ("ldwu_zex", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* ldwu_zex */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_16(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldwu_zex_gen_gen =
  ("ldwu_zex", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_LDWU(" ^ (register_to_c_gen alloc ra) ^ ", 0, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldwu_zex_disp_matcher =
  { name = "ldwu_zex_disp" ;
    pattern = AssignPattern ("ra", UnaryWidthPattern (Zex, ([2], "__dummy__"), LoadBOPattern (LittleEndian, ([2], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "rb", IntPattern (AnyInt "disp"))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and ldwu_zex_disp_printer =
  ("ldwu_zex_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "/* ldwu_zex_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_16(" ^ (expr_to_c alloc disp) ^ " + " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldwu_zex_disp_gen_gen =
  ("ldwu_zex_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDWU(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldl_matcher =
  { name = "ldl" ;
    pattern = AssignPattern ("ra", LoadBOPattern (LittleEndian, ([4], "__dummy__"), RegisterPattern "rb")) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and ldl_printer =
  ("ldl", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* ldl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_32(" ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldl_gen_gen =
  ("ldl", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_LDL(" ^ (register_to_c_gen alloc ra) ^ ", 0, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldl_disp_matcher =
  { name = "ldl_disp" ;
    pattern = AssignPattern ("ra", LoadBOPattern (LittleEndian, ([4], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "rb", IntPattern (AnyInt "disp")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and ldl_disp_printer =
  ("ldl_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "/* ldl_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc ra) ^ " = mem_load_32(" ^ (expr_to_c alloc disp) ^ " + " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and ldl_disp_gen_gen =
  ("ldl_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldl_sex_matcher =
  { name = "ldl_sex" ;
    pattern = AssignPattern ("ra", UnaryWidthPattern (Sex, ([4], "__dummy__"), LoadBOPattern (LittleEndian, ([4], "__dummy__"), RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and ldl_sex_printer =
  ("ldl_sex", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* ldl_sex */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc ra) ^ " = sex_32(mem_load_32(" ^ (register_to_c alloc rb) ^ "));" ^ " }"))
and ldl_sex_gen_gen =
  ("ldl_sex", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_LDL(" ^ (register_to_c_gen alloc ra) ^ ", 0, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let ldl_sex_disp_matcher =
  { name = "ldl_sex_disp" ;
    pattern = AssignPattern ("ra", UnaryWidthPattern (Sex, ([4], "__dummy__"), LoadBOPattern (LittleEndian, ([4], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "rb", IntPattern (AnyInt "disp"))))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and ldl_sex_disp_printer =
  ("ldl_sex_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "/* ldl_sex_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc ra) ^ " = sex_32(mem_load_32(" ^ (expr_to_c alloc disp) ^ " + " ^ (register_to_c alloc rb) ^ "));" ^ " }"))
and ldl_sex_disp_gen_gen =
  ("ldl_sex_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb" and disp = get_const_binding bindings "disp"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_LDL(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc rb) ^ "));" ^ " }"))

let stb_matcher =
  { name = "stb" ;
    pattern = StorePattern (LittleEndian, ([1], "__dummy__"), RegisterPattern "ra", RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and stb_printer =
  ("stb", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* stb */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "mem_store_8(" ^ (register_to_c alloc ra) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stb_gen_gen =
  ("stb", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_STB(" ^ (register_to_c_gen alloc rb) ^ ", 0, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let stb_disp_matcher =
  { name = "stb_disp" ;
    pattern = StorePattern (LittleEndian, ([1], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "disp")), RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and stb_disp_printer =
  ("stb_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "/* stb_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "mem_store_8(" ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc disp) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stb_disp_gen_gen =
  ("stb_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_STB(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let stw_matcher =
  { name = "stw" ;
    pattern = StorePattern (LittleEndian, ([2], "__dummy__"), RegisterPattern "ra", RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and stw_printer =
  ("stw", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* stw */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "mem_store_16(" ^ (register_to_c alloc ra) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stw_gen_gen =
  ("stw", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_STW(" ^ (register_to_c_gen alloc rb) ^ ", 0, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let stw_disp_matcher =
  { name = "stw_disp" ;
    pattern = StorePattern (LittleEndian, ([2], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "disp")), RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and stw_disp_printer =
  ("stw_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "/* stw_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "mem_store_16(" ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc disp) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stw_disp_gen_gen =
  ("stw_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_STW(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let stl_matcher =
  { name = "stl" ;
    pattern = StorePattern (LittleEndian, ([4], "__dummy__"), RegisterPattern "ra", RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and stl_printer =
  ("stl", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* stl */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "mem_store_32(" ^ (register_to_c alloc ra) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stl_gen_gen =
  ("stl", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_STL(" ^ (register_to_c_gen alloc rb) ^ ", 0, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let stl_disp_matcher =
  { name = "stl_disp" ;
    pattern = StorePattern (LittleEndian, ([4], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "disp")), RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and stl_disp_printer =
  ("stl_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "/* stl_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "mem_store_32(" ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc disp) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stl_disp_gen_gen =
  ("stl_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_STL(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let stq_matcher =
  { name = "stq" ;
    pattern = StorePattern (LittleEndian, ([8], "__dummy__"), RegisterPattern "ra", RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 2) }
and stq_printer =
  ("stq", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* stq */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "mem_store_64(" ^ (register_to_c alloc ra) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stq_gen_gen =
  ("stq", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_STQ(" ^ (register_to_c_gen alloc rb) ^ ", 0, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let stq_disp_matcher =
  { name = "stq_disp" ;
    pattern = StorePattern (LittleEndian, ([8], "__dummy__"), BinaryPattern (IntAdd, RegisterPattern "ra", IntPattern (AnyInt "disp")), RegisterPattern "rb") ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and disp = get_const_binding bindings "disp"
                 in uc_when fields (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (fun _ -> uc_return 2)) }
and stq_disp_printer =
  ("stq_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "/* stq_disp */ { assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "mem_store_64(" ^ (register_to_c alloc ra) ^ " + " ^ (expr_to_c alloc disp) ^ ", " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and stq_disp_gen_gen =
  ("stq_disp", (fun alloc bindings ->
              let ra = get_register_binding bindings alloc "ra" and disp = get_const_binding bindings "disp" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_zero_or_full_p (BinaryWidth (AShiftR, 8, disp, int_literal_expr (16L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_STQ(" ^ (register_to_c_gen alloc rb) ^ ", " ^ (expr_to_c alloc disp) ^ " & 0xffff, " ^ (register_to_c_gen alloc ra) ^ "));" ^ " }"))

let cmpeq_matcher =
  { name = "cmpeq" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (IntEqual, RegisterPattern "ra", RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmpeq_printer =
  ("cmpeq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* cmpeq */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " == " ^ (register_to_c alloc rb) ^ ");" ^ " }"))
and cmpeq_gen_gen =
  ("cmpeq", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPEQ(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmpeq_imm_matcher =
  { name = "cmpeq_imm" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (IntEqual, RegisterPattern "ra", IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and cmpeq_imm_printer =
  ("cmpeq_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* cmpeq_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = (" ^ (register_to_c alloc ra) ^ " == " ^ (expr_to_c alloc i) ^ ");" ^ " }"))
and cmpeq_imm_gen_gen =
  ("cmpeq_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_CMPEQ_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmplt_matcher =
  { name = "cmplt" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (LessS, RegisterPattern "ra", RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmplt_printer =
  ("cmplt", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* cmplt */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (((sword_64)" ^ (register_to_c alloc ra) ^ ") < ((sword_64)" ^ (register_to_c alloc rb) ^ "));" ^ " }"))
and cmplt_gen_gen =
  ("cmplt", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPLT(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmplt_imm_matcher =
  { name = "cmplt_imm" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (LessS, RegisterPattern "ra", IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and cmplt_imm_printer =
  ("cmplt_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* cmplt_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = (((sword_64)" ^ (register_to_c alloc ra) ^ ") < ((sword_64)" ^ (expr_to_c alloc i) ^ "));" ^ " }"))
and cmplt_imm_gen_gen =
  ("cmplt_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_CMPLT_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmplt_31_matcher =
  { name = "cmplt_31" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (LessS, IntPattern (TheInt (int_literal_expr (0L))), RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmplt_31_printer =
  ("cmplt_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* cmplt_31 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (((sword_64)0LL) < ((sword_64)" ^ (register_to_c alloc rb) ^ "));" ^ " }"))
and cmplt_31_gen_gen =
  ("cmplt_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPLT(31, " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmple_matcher =
  { name = "cmple" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, make_leis_pattern (RegisterPattern "ra") (RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmple_printer =
  ("cmple", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* cmple */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (((sword_64)" ^ (register_to_c alloc ra) ^ ") <= ((sword_64)" ^ (register_to_c alloc rb) ^ "));" ^ " }"))
and cmple_gen_gen =
  ("cmple", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPLE(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmple_imm_matcher =
  { name = "cmple_imm" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, make_leis_pattern (RegisterPattern "ra") (IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and cmple_imm_printer =
  ("cmple_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* cmple_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = (((sword_64)" ^ (register_to_c alloc ra) ^ ") <= ((sword_64)" ^ (expr_to_c alloc i) ^ "));" ^ " }"))
and cmple_imm_gen_gen =
  ("cmple_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_CMPLE_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmple_31_matcher =
  { name = "cmple_31" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, make_leis_pattern (IntPattern (TheInt (int_literal_expr (0L)))) (RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmple_31_printer =
  ("cmple_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* cmple_31 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = (((sword_64)0LL) <= ((sword_64)" ^ (register_to_c alloc rb) ^ "));" ^ " }"))
and cmple_31_gen_gen =
  ("cmple_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPLE(31, " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmpult_matcher =
  { name = "cmpult" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (LessU, RegisterPattern "ra", RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmpult_printer =
  ("cmpult", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* cmpult */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " < " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and cmpult_gen_gen =
  ("cmpult", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPULT(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmpult_imm_matcher =
  { name = "cmpult_imm" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (LessU, RegisterPattern "ra", IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and cmpult_imm_printer =
  ("cmpult_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* cmpult_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " < " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and cmpult_imm_gen_gen =
  ("cmpult_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_CMPULT_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmpult_31_matcher =
  { name = "cmpult_31" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, BinaryPattern (LessU, IntPattern (TheInt (int_literal_expr (0L))), RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmpult_31_printer =
  ("cmpult_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* cmpult_31 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = 0LL < " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and cmpult_31_gen_gen =
  ("cmpult_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPULT(31, " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmpule_matcher =
  { name = "cmpule" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, make_leiu_pattern (RegisterPattern "ra") (RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmpule_printer =
  ("cmpule", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "/* cmpule */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " <= " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and cmpule_gen_gen =
  ("cmpule", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPULE(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmpule_imm_matcher =
  { name = "cmpule_imm" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, make_leiu_pattern (RegisterPattern "ra") (IntPattern (AnyInt "i")))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = [] and i = get_const_binding bindings "i"
                 in uc_when fields (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (fun _ -> uc_return 1)) }
and cmpule_imm_printer =
  ("cmpule_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "/* cmpule_imm */ { assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ (register_to_c alloc rs) ^ " = " ^ (register_to_c alloc ra) ^ " <= " ^ (expr_to_c alloc i) ^ ";" ^ " }"))
and cmpule_imm_gen_gen =
  ("cmpule_imm", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and ra = get_register_binding bindings alloc "ra" and i = get_const_binding bindings "i"
              in "{ bt_assert(" ^ (expr_to_c [] (and_expr (make_int_zero_p (BinaryWidth (LShiftR, 8, i, int_literal_expr (8L)))) (ConditionConst true))) ^ "); " ^ "emit(COMPOSE_CMPULE_IMM(" ^ (register_to_c_gen alloc ra) ^ ", " ^ (expr_to_c alloc i) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let cmpule_31_matcher =
  { name = "cmpule_31" ;
    pattern = AssignPattern ("rs", UnaryPattern (ConditionToInt, make_leiu_pattern (IntPattern (TheInt (int_literal_expr (0L)))) (RegisterPattern "rb"))) ;
    matcher = (fun fields bindings ->
                 let __dummy__ = []
                 in uc_return 1) }
and cmpule_31_printer =
  ("cmpule_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "/* cmpule_31 */ { assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ (register_to_c alloc rs) ^ " = 0LL <= " ^ (register_to_c alloc rb) ^ ";" ^ " }"))
and cmpule_31_gen_gen =
  ("cmpule_31", (fun alloc bindings ->
              let rs = get_register_binding bindings alloc "rs" and rb = get_register_binding bindings alloc "rb"
              in "{ bt_assert(" ^ (expr_to_c [] (ConditionConst true)) ^ "); " ^ "emit(COMPOSE_CMPULE(31, " ^ (register_to_c_gen alloc rb) ^ ", " ^ (register_to_c_gen alloc rs) ^ "));" ^ " }"))

let alpha_matchers = [ addq_matcher ; addl_matcher ; addl_imm_matcher ; addl_31_matcher ; eqv_matcher ; eqv_imm_matcher ; lda_matcher ; lda_31_matcher ; ldah_matcher ; ldah_31_matcher ; load_int_matcher ; mov_matcher ; and_matcher ; and_imm_matcher ; bic_imm_for_and_matcher ; zapnot_imm_matcher ; bis_matcher ; bis_imm_matcher ; bic_matcher ; bic_imm_matcher ; mull_matcher ; mull_imm_matcher ; mulq_matcher ; mulq_imm_matcher ; neg_matcher ; negl_matcher ; negq_matcher ; ornot_matcher ; sll_matcher ; sll_imm_matcher ; sra_matcher ; sra_imm_matcher ; srl_matcher ; srl_imm_matcher ; zapnot_imm_srl_matcher ; zapnot_imm_srl_imm_matcher ; srl_matcher ; srl_imm_matcher ; extbl_imm_8_matcher ; extbl_imm_4_matcher ; extbl_imm_4_3_matcher ; extwl_imm_8_matcher ; extwl_imm_4_matcher ; extwl_imm_4_2_matcher ; sextb_matcher ; sextw_matcher ; subl_matcher ; subl_imm_matcher ; subq_matcher ; subq_imm_matcher ; xor_matcher ; xor_imm_matcher ; zapnot_imm_1_matcher ; zapnot_imm_3_matcher ; zapnot_imm_15_matcher ; zapnot_imm_sll_srl_imm_bis_matcher ; extract_and_imm_matcher ; extract_zapnot_imm_matcher ; extract_srl_matcher ; extract_extbl_imm_matcher ; extract_extwl_imm_matcher ; extract_sll_srl_matcher ; insert_full_matcher ; ldbu_matcher ; ldbu_disp_matcher ; ldbu_zex_matcher ; ldbu_zex_disp_matcher ; ldwu_matcher ; ldwu_disp_matcher ; ldwu_zex_matcher ; ldwu_zex_disp_matcher ; ldl_matcher ; ldl_disp_matcher ; ldl_sex_matcher ; ldl_sex_disp_matcher ; stb_matcher ; stb_disp_matcher ; stw_matcher ; stw_disp_matcher ; stl_matcher ; stl_disp_matcher ; stq_matcher ; stq_disp_matcher ; cmpeq_matcher ; cmpeq_imm_matcher ; cmplt_matcher ; cmplt_imm_matcher ; cmplt_31_matcher ; cmple_matcher ; cmple_imm_matcher ; cmple_31_matcher ; cmpult_matcher ; cmpult_imm_matcher ; cmpult_31_matcher ; cmpule_matcher ; cmpule_imm_matcher ; cmpule_31_matcher ]
let alpha_printers = [ addq_printer ; addl_printer ; addl_imm_printer ; addl_31_printer ; eqv_printer ; eqv_imm_printer ; lda_printer ; lda_31_printer ; ldah_printer ; ldah_31_printer ; load_int_printer ; mov_printer ; and_printer ; and_imm_printer ; bic_imm_for_and_printer ; zapnot_imm_printer ; bis_printer ; bis_imm_printer ; bic_printer ; bic_imm_printer ; mull_printer ; mull_imm_printer ; mulq_printer ; mulq_imm_printer ; neg_printer ; negl_printer ; negq_printer ; ornot_printer ; sll_printer ; sll_imm_printer ; sra_printer ; sra_imm_printer ; srl_printer ; srl_imm_printer ; zapnot_imm_srl_printer ; zapnot_imm_srl_imm_printer ; srl_printer ; srl_imm_printer ; extbl_imm_8_printer ; extbl_imm_4_printer ; extbl_imm_4_3_printer ; extwl_imm_8_printer ; extwl_imm_4_printer ; extwl_imm_4_2_printer ; sextb_printer ; sextw_printer ; subl_printer ; subl_imm_printer ; subq_printer ; subq_imm_printer ; xor_printer ; xor_imm_printer ; zapnot_imm_1_printer ; zapnot_imm_3_printer ; zapnot_imm_15_printer ; zapnot_imm_sll_srl_imm_bis_printer ; extract_and_imm_printer ; extract_zapnot_imm_printer ; extract_srl_printer ; extract_extbl_imm_printer ; extract_extwl_imm_printer ; extract_sll_srl_printer ; insert_full_printer ; ldbu_printer ; ldbu_disp_printer ; ldbu_zex_printer ; ldbu_zex_disp_printer ; ldwu_printer ; ldwu_disp_printer ; ldwu_zex_printer ; ldwu_zex_disp_printer ; ldl_printer ; ldl_disp_printer ; ldl_sex_printer ; ldl_sex_disp_printer ; stb_printer ; stb_disp_printer ; stw_printer ; stw_disp_printer ; stl_printer ; stl_disp_printer ; stq_printer ; stq_disp_printer ; cmpeq_printer ; cmpeq_imm_printer ; cmplt_printer ; cmplt_imm_printer ; cmplt_31_printer ; cmple_printer ; cmple_imm_printer ; cmple_31_printer ; cmpult_printer ; cmpult_imm_printer ; cmpult_31_printer ; cmpule_printer ; cmpule_imm_printer ; cmpule_31_printer ]
let alpha_gen_gens = [ addq_gen_gen ; addl_gen_gen ; addl_imm_gen_gen ; addl_31_gen_gen ; eqv_gen_gen ; eqv_imm_gen_gen ; lda_gen_gen ; lda_31_gen_gen ; ldah_gen_gen ; ldah_31_gen_gen ; load_int_gen_gen ; mov_gen_gen ; and_gen_gen ; and_imm_gen_gen ; bic_imm_for_and_gen_gen ; zapnot_imm_gen_gen ; bis_gen_gen ; bis_imm_gen_gen ; bic_gen_gen ; bic_imm_gen_gen ; mull_gen_gen ; mull_imm_gen_gen ; mulq_gen_gen ; mulq_imm_gen_gen ; neg_gen_gen ; negl_gen_gen ; negq_gen_gen ; ornot_gen_gen ; sll_gen_gen ; sll_imm_gen_gen ; sra_gen_gen ; sra_imm_gen_gen ; srl_gen_gen ; srl_imm_gen_gen ; zapnot_imm_srl_gen_gen ; zapnot_imm_srl_imm_gen_gen ; srl_gen_gen ; srl_imm_gen_gen ; extbl_imm_8_gen_gen ; extbl_imm_4_gen_gen ; extbl_imm_4_3_gen_gen ; extwl_imm_8_gen_gen ; extwl_imm_4_gen_gen ; extwl_imm_4_2_gen_gen ; sextb_gen_gen ; sextw_gen_gen ; subl_gen_gen ; subl_imm_gen_gen ; subq_gen_gen ; subq_imm_gen_gen ; xor_gen_gen ; xor_imm_gen_gen ; zapnot_imm_1_gen_gen ; zapnot_imm_3_gen_gen ; zapnot_imm_15_gen_gen ; zapnot_imm_sll_srl_imm_bis_gen_gen ; extract_and_imm_gen_gen ; extract_zapnot_imm_gen_gen ; extract_srl_gen_gen ; extract_extbl_imm_gen_gen ; extract_extwl_imm_gen_gen ; extract_sll_srl_gen_gen ; insert_full_gen_gen ; ldbu_gen_gen ; ldbu_disp_gen_gen ; ldbu_zex_gen_gen ; ldbu_zex_disp_gen_gen ; ldwu_gen_gen ; ldwu_disp_gen_gen ; ldwu_zex_gen_gen ; ldwu_zex_disp_gen_gen ; ldl_gen_gen ; ldl_disp_gen_gen ; ldl_sex_gen_gen ; ldl_sex_disp_gen_gen ; stb_gen_gen ; stb_disp_gen_gen ; stw_gen_gen ; stw_disp_gen_gen ; stl_gen_gen ; stl_disp_gen_gen ; stq_gen_gen ; stq_disp_gen_gen ; cmpeq_gen_gen ; cmpeq_imm_gen_gen ; cmplt_gen_gen ; cmplt_imm_gen_gen ; cmplt_31_gen_gen ; cmple_gen_gen ; cmple_imm_gen_gen ; cmple_31_gen_gen ; cmpult_gen_gen ; cmpult_imm_gen_gen ; cmpult_31_gen_gen ; cmpule_gen_gen ; cmpule_imm_gen_gen ; cmpule_31_gen_gen ]
