(*
 * normal_form.ml
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

(* FIXME: implement
     -(x+c)    -> -x+[-c]
     -(-(x)+c) -> x+[-c]
*)

let rec normalize_expr expr =
  let rec normalize_step expr =
    let nexpr = apply_to_expr_subs normalize_step (cfold_expr [] expr)
    in match nexpr with
	Binary (IntAdd, c, x) when (is_const c) && (not (is_const x)) ->
	  (* c+x -> x+c *)
	  Binary (IntAdd, x, c)
      | Binary (IntAdd, Binary (IntAdd, x, c), y) when (not (is_const x)) && (is_const c) && (not (is_const y)) ->
	  (* (x+c)+y -> (x+y)+c *)
	  Binary (IntAdd, Binary (IntAdd, x, y), c)
      | Binary (IntAdd, Binary (IntAdd, x, c), d) when (not (is_const x)) && (is_const c) && (is_const d) ->
	  (* (x+c)+d -> x+[c+d] *)
	  Binary (IntAdd, x, cfold_expr [] (Binary (IntAdd, c, d)))
      | Binary (IntAdd, x, Binary (IntAdd, y, c)) when (not (is_const x)) && (not (is_const y)) && (is_const c) ->
	  (* x+(y+c) -> (x+y)+c *)
	  Binary (IntAdd, Binary (IntAdd, x, y), c)
      | Binary (IntAdd, x, Unary (IntNeg, Binary (IntAdd, y, c))) when (not (is_const x)) && (not (is_const y)) && (is_const c) ->
	  (* x+(-(y+c) )-> (x+(-y))+[-c] *)
	  Binary (IntAdd, Binary (IntAdd, x, Unary (IntNeg, y)), cfold_expr [] (Unary (IntNeg, c)))
      | _ -> nexpr
  in let nexpr = normalize_step expr
  in if expr = nexpr then
      expr
    else
      normalize_expr nexpr
