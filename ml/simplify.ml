(*
 * simplify.ml
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

open Expr
open Cond_monad
open Matcher
open Simplifiers

let rec simplify_expr fields expr =
  if is_const (cfold_expr fields expr) then
    (cm_return expr)
  else
    cm_bind (apply_to_expr_subs_with_monad cm_return cm_bind (simplify_expr fields) expr)
      (fun sexpr ->
	let rec try_simplifiers simplifiers =
	  match simplifiers with
	    { pattern = pattern ; apply = apply } :: rest ->
	      cm_if_success (match_expr fields sexpr pattern)
		(fun bindings ->
		  cm_return (apply sexpr bindings))
	        (fun _ -> (try_simplifiers rest))
	  | [] -> cm_return sexpr
	in try_simplifiers simplifiers)

let simplify_stmt fields =
  apply_to_stmt_subs_with_monad cm_return cm_bind (simplify_expr fields)
