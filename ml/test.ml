open List
open Int64

open Expr
open Target_alpha
open Matcher
open Simplify
open Pruner

let make_mask begin_bit end_bit =
  If (BinaryWidth (LessU, 8, end_bit, begin_bit),
      Binary(BitOr, Binary(ShiftL, IntConst (IntLiteral minus_one), begin_bit),
	     BinaryWidth (LShiftR, 8, IntConst (IntLiteral minus_one), Binary (IntSub, IntConst (IntLiteral 63L), end_bit))),
      Binary(ShiftL, BinaryWidth (LShiftR, 8, IntConst (IntLiteral minus_one),
				  Binary (IntSub, IntConst (IntLiteral 63L), Binary (IntSub, end_bit, begin_bit))),
	     begin_bit))

let make_rotl4 x a =
  Binary (BitOr, Binary (ShiftL, x, a), BinaryWidth (LShiftR, 4, x, (Binary (IntSub, IntConst (IntLiteral 32L), a))))

let make_ppc_and ra rs rb =
  Assign (ra, Binary (BitAnd, Register rs, Register rb))

let make_ppc_andc ra rs rb =
  Assign (ra, Binary (BitAnd, Register rs, Unary (BitNeg, Register rb)))

let make_ppc_rlwinm ra rs sh mb me =
  Assign (ra, Binary (BitAnd, make_rotl4 rs sh,
		      make_mask (Binary (IntSub, IntConst (IntLiteral 31L), me))
			(Binary (IntSub, IntConst (IntLiteral 31L), mb))))

let main () =
  let r1 = (1, Int)
  and r2 = (2, Int)
  and r3 = (3, Int)
  in let stmt1 = make_ppc_andc r1 r2 r3
  and stmt2 = Assign (r1, Binary (BitAnd, Unary (BitNeg, Register r2), Register r3))
  and stmt3 = Assign (r1, (make_rotl4 (Register r2) (IntConst (IntLiteral 8L))))
  and stmt4 = Assign (r1, IntConst (IntLiteral 8000000L))
  and stmt5 = Assign (r1, make_mask (IntConst (IntLiteral 8L)) (IntConst (IntLiteral 15L)))
  and stmt6 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntLiteral 16L)) (IntConst (IntLiteral 0L)) (IntConst (IntLiteral 31L))
  in let stmt = prune_stmt (simplify_stmt (cfold_stmt stmt6))
  in print_stmt stmt ;
  let (best_stmt_match, best_sub_matches_alist) = recursively_match_stmt stmt alpha_insns
  in print_whole_match best_stmt_match best_sub_matches_alist;;

main ();;
exit 0;;
