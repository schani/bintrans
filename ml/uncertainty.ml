(*
 * uncertainty.ml
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

(*** uncertainty ***)

type 'a uncertainty =
    Maybe of 'a * expr
  | Certainly of 'a

let uncert_value u =
  match u with
      Maybe (x, _) -> x
    | Certainly x -> x

let uncert_combine x y value =
  match (x, y) with
      (Certainly _, Certainly _) -> Certainly value
    | (Maybe (_, cond), Certainly _) -> Maybe (value, cond)
    | (Certainly _, Maybe (_, cond)) -> Maybe (value, cond)
    | (Maybe (_, cond1), Maybe (_, cond2)) -> Maybe (value, and_expr cond1 cond2)

let uncert_less x y =
  (uncert_value x) < (uncert_value y)

let uncert_min x y =
  if uncert_less x y then
    uncert_combine x y (uncert_value x)
  else
    uncert_combine x y (uncert_value y)

let uncert_add x y =
  uncert_combine x y ((uncert_value x) + (uncert_value y))

let print_uncert printer u =
  match u with
      Maybe (x, cond) -> print_string "maybe " ; printer x ; print_string " (if " ; print_expr cond ; print_string ")"
    | Certainly x -> print_string "certainly " ; printer x

(*** uncertainty monad ***)

type 'a uncertainty_monad = 'a uncertainty condition_monad

let uc_return expr = cm_return (Certainly expr)
let uc_fail = cm_fail

let uc_bind = cm_bind
let uc_if = cm_if

let uc_when fields condition consequent =
  if is_register_const fields condition then
    let ccond = cfold_expr fields condition
    in if is_const ccond then
	cm_when fields condition consequent
      else
	match consequent () with
	    CMSuccess (Certainly expr, conds) -> CMSuccess (Maybe (expr, condition), conds)
	  | CMSuccess (Maybe (expr, cond), conds) -> CMSuccess (Maybe (expr, and_expr condition cond), conds)
	  | CMFailureWithReason (conds, fcond) -> CMFailureWithReason (conds, fcond)
	  | CMFailure -> CMFailure
  else
    raise Expression_not_const
