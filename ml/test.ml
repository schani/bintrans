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

let rec repeat_until_fixpoint fn data =
  let ndata = fn data
  in if ndata = data then
    data
  else
    repeat_until_fixpoint fn ndata

let explore_all_fields stmt fields =
  let rec explore fields_so_far rest_fields stmts_so_far =
    match rest_fields with
      [] ->
	let new_stmt = repeat_until_fixpoint (fun stmt -> prune_stmt fields_so_far (simplify_stmt fields_so_far stmt)) stmt
	in if mem new_stmt stmts_so_far then
	  stmts_so_far
	else
	  (print_stmt new_stmt ; (new_stmt :: stmts_so_far))
    | (name, min, max) :: rest_fields when (compare min max) < 0 ->
	let stmts_so_far = explore ((name, min) :: fields_so_far) rest_fields stmts_so_far
	in explore fields_so_far ((name, (add min one), max) :: rest_fields) stmts_so_far
    | _ -> stmts_so_far
  in
    explore [] fields []
      

let main () =
  let r1 = (1, Int)
  and r2 = (2, Int)
  and r3 = (3, Int)
  in let stmt1 = make_ppc_andc r1 r2 r3
  and stmt2 = Assign (r1, Binary (BitAnd, Unary (BitNeg, Register r2), Register r3))
  and stmt3 = Assign (r1, (make_rotl4 (Register r2) (IntConst (IntLiteral 8L))))
  and stmt4 = Assign (r1, IntConst (IntLiteral 8000000L))
  and stmt5 = Assign (r1, make_mask (IntConst (IntLiteral 8L)) (IntConst (IntLiteral 15L)))
  and stmt6 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
  and fields = [("sh", 16L); ("mb", 16L); ("me", 31L)]
  in let stmt = repeat_until_fixpoint (fun stmt -> prune_stmt fields (simplify_stmt fields stmt)) stmt6
  in print_stmt stmt ;
  let (best_stmt_match, best_sub_matches_alist) = recursively_match_stmt fields stmt alpha_insns
  in print_whole_match best_stmt_match best_sub_matches_alist ;
  let stmts = explore_all_fields stmt6 [("sh", 0L, 32L); ("mb", 0L, 32L); ("me", 0L, 32L)]
  in print_newline () ;;

main ();;
exit 0;;
