(*
 * cgen.ml
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

open Int64
open List
open String

open Utils
open Expr
open Matcher

exception Register_not_bound

type allocation = (expr * int) list

type printer = (string * ((input_name * int64) list -> allocation -> binding list -> string))

(*** allocation of intermediate registers ***)

let make_allocation best_matches_alist match_datas =
  let rec make_for_match_data match_data =
    make match_data.bindings
  and make_for_match_datas match_datas so_far =
    match match_datas with
	[] -> so_far
      | m :: rest ->
	  let so_far = make_for_match_data m so_far
	  in make_for_match_datas rest so_far
  and make_for_expr_matches expr_matches =
    make_for_match_datas (map (fun m -> m.expr_match_data) expr_matches)
  and make_for_binding binding so_far =
    match binding with
	RegisterBinding (_, IntermediateRegister expr) ->
	  if mem expr so_far then
	    so_far
	  else
	    make_for_expr_matches (assoc expr best_matches_alist) (expr :: so_far)
      | ExprBinding (_, expr) ->
	  if mem expr so_far then
	    so_far
	  else
	    make_for_expr_matches (assoc expr best_matches_alist) so_far
      | _ -> so_far
  and make bindings so_far =
    match bindings with
	[] -> so_far
      | binding :: rest ->
	  let so_far = make_for_binding binding so_far
	  in make rest so_far
  in map_int (fun x i -> (x, i)) (make_for_match_datas match_datas []) 1

let lookup_intermediate_reg allocation expr =
  try
    assoc expr allocation
  with
      Not_found -> raise Register_not_bound

(*** generating code for exprs ***)

let int_const_to_c int_const =
  match int_const with
      IntLiteral const -> (to_string const) ^ "LL"
    | IntField input_name -> "((word_64)PPC_FIELD_" ^ (uppercase input_name) ^ ")"

let register_to_c allocation reg =
  match reg with
      GuestRegister (IntLiteral num, _) -> "guest_reg_" ^ (to_string num)
    | GuestRegister (IntField name, _) -> "guest_reg_" ^ name
    | HostRegister (num, _) -> "host_reg_" ^ (string_of_int num)
    | IntermediateRegister expr -> "interm_reg_" ^ (string_of_int (lookup_intermediate_reg allocation expr))
    | LetRegister (name, value_type, width) -> "let_reg_" ^ name

let expr_to_c allocation expr =
  let unary_to_c op arg =
    "unary_" ^ (unary_op_name op) ^ "(" ^ arg ^ ")"
  and unary_width_to_c op width arg =
    "unary_" ^ (unary_width_op_name op) ^ "_" ^ (string_of_int width) ^ "(" ^ arg ^ ")"
  and binary_to_c op arg1 arg2 =
    "binary_" ^ (binary_op_name op) ^ "(" ^ arg1 ^ "," ^ arg2 ^ ")"
  and binary_width_to_c op width arg1 arg2 =
    "binary_" ^ (binary_width_op_name op) ^ "_" ^ (string_of_int width) ^ "(" ^ arg1 ^ "," ^ arg2 ^ ")"
  and ternary_width_to_c op width arg1 arg2 arg3 =
    "ternary_" ^ (ternary_width_op_name op) ^ "_" ^ (string_of_int width) ^ "(" ^ arg1 ^ "," ^ arg2 ^ "," ^ arg3 ^ ")"
  in let rec expr_to_c expr =
      match expr with
	  IntConst int_const -> int_const_to_c int_const
	| FloatConst const -> string_of_float const
	| ConditionConst false -> "0"
	| ConditionConst true -> "1"
	| Register reg -> register_to_c allocation reg
	| LoadBO (byte_order, width, addr) ->
	    "load_" ^ (byte_order_string byte_order) ^ (string_of_int width) ^ "(" ^ (expr_to_c addr) ^ ")"
	| Unary (op, arg) -> unary_to_c op (expr_to_c arg)
	| UnaryWidth (op, width, arg) -> unary_width_to_c op width (expr_to_c arg)
	| Binary (op, arg1, arg2) -> binary_to_c op (expr_to_c arg1) (expr_to_c arg2)
	| BinaryWidth (op, width, arg1, arg2) -> binary_width_to_c op width (expr_to_c arg1) (expr_to_c arg2)
	| TernaryWidth (op, width, arg1, arg2, arg3) -> ternary_width_to_c op width (expr_to_c arg1) (expr_to_c arg2) (expr_to_c arg3)
	| Extract (expr, start, length) ->
	    "bit_extract(" ^ (expr_to_c expr) ^ "," ^ (expr_to_c start) ^ "," ^ (expr_to_c length) ^ ")"
	| Insert (arg1, arg2, start, length) ->
	    "bit_insert(" ^ (expr_to_c arg1) ^ "," ^ (expr_to_c arg2) ^ "," ^ (int_const_to_c start) ^ "," ^ (int_const_to_c length) ^ ")"
	| If (condition, cons, alt) ->
	    "(" ^ (expr_to_c condition) ^ "?" ^ (expr_to_c cons) ^ ":" ^ (expr_to_c alt) ^ ")"
	| UserOp (name, args) ->
	    "userop_" ^ name ^ "(" ^ (join_strings ", " (map expr_to_c args)) ^ ")"
  in expr_to_c expr

let rec stmt_to_c allocation stmt =
  match stmt with
      Store (byte_order, width, addr, value) ->
	"store_" ^ (byte_order_string byte_order) ^ "_" ^ (string_of_int width) ^ "("
	^ (expr_to_c allocation addr) ^ "," ^ (expr_to_c allocation value) ^ ");"
    | Assign (reg, src) ->
	(register_to_c allocation reg) ^ "=" ^ (expr_to_c allocation src) ^ ";"
    | Let (name, _, rhs, sub) ->
	(* FIXME: not only integer types are possible! *)
	"{ word_64 let_reg_" ^ name ^ " = " ^ (expr_to_c allocation rhs) ^ ";\n" ^ (stmt_to_c allocation sub) ^ "\n}"
    | Seq (sub1, sub2) ->
	"{ " ^ (stmt_to_c allocation sub1) ^ "\n" ^ (stmt_to_c allocation sub2) ^ "\n}"
    | IfStmt (cond, cons, alt) ->
	"if (" ^ (expr_to_c allocation cond) ^ ")\n" ^ (stmt_to_c allocation cons) ^ "\nelse\n" ^ (stmt_to_c allocation alt)

(*** generating generators ***)

let register_to_c_gen = register_to_c
