open List

open Expr
open Cgen

type 'a switch =
    Case of 'a
  | Decision of expr list * 'a switch * 'a switch
  | MustNotHappen

let rec switch_cases conds_list =
  let filter_cond cond =
    filter (fun (cs, _) -> mem cond cs) conds_list
  in match conds_list with
    [] -> MustNotHappen
  | [(_, a)] -> Case a
  | (conds, a) :: rest ->
      try
	let conds = filter (fun c ->
	                     (for_all (fun (cs, _) ->
			                 exists
			                   (fun c2 -> c = c2 || c = (not_expr c2))
			                   cs)
	                        rest) &&
	                     (exists (fun (cs, _) ->
			                exists
			                   (fun c2 -> c = (not_expr c2))
			                   cs)
			        rest))
	                   conds
	in let cond = fold_left (fun c1 c2 -> if (expr_size c1) < (expr_size c2) then c1 else c2) (hd conds) (tl conds)
	in Decision ([cond],
		     switch_cases (filter_cond cond),
		     switch_cases (filter_cond (not_expr cond)))
      with
	Failure _ ->
	  Decision (conds,
		    Case a,
		    switch_cases rest)

let rec print_switch printer switch =
  match switch with
    Case a -> printer a
  | Decision (conds, consequent, alternative) ->
      print_string "if (" ;
      iter (fun c -> print_string (expr_to_c [] c) ; print_string " && ") conds ;
      print_string "1)" ; print_newline () ;
      print_string "{" ; print_newline () ;
      print_switch printer consequent ;
      print_string "} else {" ; print_newline () ;
      print_switch printer alternative ;
      print_string "}" ; print_newline ()
  | MustNotHappen ->
      print_string "assert(0);" ; print_newline ()
