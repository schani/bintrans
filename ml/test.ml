open List
open Int64

open Expr
open Cond_monad
open Target_alpha
open Matcher
open Explorer

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

let make_ppc_mask mb me =
  make_mask (Binary (IntSub, IntConst (IntLiteral 31L), me))
    (Binary (IntSub, IntConst (IntLiteral 31L), mb))

let make_ppc_rlwinm ra rs sh mb me =
  Assign (ra, Binary (BitAnd, make_rotl4 rs sh, make_ppc_mask mb me))

(*
let rec repeat_until_fixpoint fn data =
  let ndata = fn data
  in if ndata = data then
    data
  else
    repeat_until_fixpoint fn ndata
*)

let rec print_target_insns best_matches_alist match_data =
  
  let bindings  = filter (fun b -> match b with
			      ExprBinding _ -> true
			    | _ -> false) match_data.bindings
  in
    print_string match_data.target_insn.name ;
    match bindings with
	[] -> ()
      | _ :: _ ->
	  print_string "(" ;
	  iter (fun b ->
		  match b with
		      ExprBinding (name, expr) ->
			print_string name ; print_string " = " ;
			print_target_insns best_matches_alist (assoc expr best_matches_alist).expr_match_data ;
			print_string " ; "
		    | _ -> raise Wrong_binding)
	    bindings ;
	  print_string ")"

let test_expore () =
  let r1 = (1, Int)
  and r2 = (2, Int)
  and r3 = (3, Int)
  in let stmt1 = make_ppc_andc r1 r2 r3
  and stmt2 = Assign (r1, Binary (BitAnd, Unary (BitNeg, Register r2), Register r3))
  and stmt3 = Assign (r1, (make_rotl4 (Register r2) (IntConst (IntLiteral 8L))))
  and stmt4 = Assign (r1, IntConst (IntLiteral 8000000L))
  and stmt5 = Assign (r1, make_mask (IntConst (IntLiteral 8L)) (IntConst (IntLiteral 15L)))
  and stmt6 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
(*  and fields = [("sh", 16L); ("mb", 16L); ("me", 31L)]
  in let stmt = repeat_until_fixpoint (fun stmt -> (cm_value (prune_stmt fields (cm_value (simplify_stmt fields stmt))))) stmt6
  in print_stmt stmt ;
  let (best_stmt_match, best_sub_matches_alist) = recursively_match_stmt fields stmt alpha_insns
  in print_whole_match best_stmt_match best_sub_matches_alist ;
*)
  in let stmt_forms = explore_all_fields stmt6 [("sh", 0L, 32L); ("mb", 0L, 32L); ("me", 0L, 32L)] alpha_insns
  in iter (fun form ->
	     print_stmt form.stmt ;
	     iter (fun form_match ->
		     print_string "  " ;
		     print_target_insns form_match.best_sub_matches form_match.match_data ;
		     print_newline () ;
(*
		     iter (fun c ->
			     print_expr c ; print_newline ()) form_match.match_conditions
*)
   )
	       form.matches ;
	     print_newline ()) stmt_forms

let main () =
  let r1 = (1, Int)
  and r2 = (2, Int)
  and r3 = (3, Int)
  in let stmt1 = Assign (r1, Binary (BitAnd, Binary (ShiftL, Register r2, IntConst (IntField "sh")),
				     (make_mask (int_literal_expr 0L) (int_literal_expr 15L))))
     and stmt2 = make_ppc_rlwinm r1 (Register r2) (IntConst (IntField "sh")) (IntConst (IntField "mb")) (IntConst (IntField "me"))
     and fields = [("sh", 0L); ("mb", 3L); ("me", 30L)]
  in let (stmt, conds) = cm_yield (simplify_and_prune_stmt_until_fixpoint fields stmt2)
  in let conds = simplify_conditions conds
  in print_stmt stmt2 ; print_string "->\n" ;
    print_stmt (cfold_stmt [] stmt) ; print_string "=\n" ;
    print_stmt (cfold_stmt fields stmt) ; print_string "when\n" ;
    iter (fun x -> print_expr x ; print_newline ()) conds ;;

(* main ();; *)

test_expore ();;

exit 0;;
