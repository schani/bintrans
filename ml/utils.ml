(*
 * utils.ml
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

let map0_int f start stop =
  let rec map i =
    if i > stop then
      []
    else
      (f i) :: map (i + 1)
  in map start

let map_int f l start =
  let rec map l i =
    match l with
	[] -> []
      | x :: rest ->
	  (f x i) :: (map rest (i + 1))
  in map l start

let join_strings sep strs =
  match strs with
      [] -> ""
    | first :: rest ->
	fold_left (fun a b -> a ^ sep ^ b) first rest

let rec uniq lst =
  match lst with
      [] -> []
    | x :: xs ->
	let uxs = uniq xs
	in if mem x uxs then
	    uxs
	  else
	    x :: uxs
