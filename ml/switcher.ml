(*
 * switcher.ml
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

open List

open Expr
open Cgen

type 'a switch =
    Case of 'a
  | Decision of expr list * 'a switch * 'a switch
  | MustNotHappen

let rec switch_cases_generic simplify conds_list =
  let filter_cond cond =
    filter (fun (cs, _) -> mem cond cs) conds_list
  in match conds_list with
    [] -> MustNotHappen
  | [(_, a)] -> Case a
  | (conds, a) :: rest ->
      if simplify then
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
		       switch_cases_generic simplify (filter_cond cond),
		       switch_cases_generic simplify (filter_cond (not_expr cond)))
	with
	    Failure _ ->
	      Decision (conds,
			Case a,
			switch_cases_generic simplify rest)
      else
	Decision (conds,
		  Case a,
		  switch_cases_generic simplify rest)

let switch_cases conds_list =
  switch_cases_generic true conds_list
and switch_cases_no_simplify conds_list =
  switch_cases_generic false conds_list

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
