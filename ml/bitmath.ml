(*
 * bitmath.ml
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

(*** single bits ***)

let is_bit_set int64 bit_no =
  (logand (shift_right_logical int64 bit_no) 1L) = 1L

(*** sign/zero extension ***)

let sex width arg =
  shift_right (shift_left arg (64 - width)) (64 - width)

let zex width arg =
  shift_right_logical (shift_left arg (64 - width)) (64 - width)

(*** shifting ***)

let bad_shift_amount a =
  (compare a 0L) < 0 || (compare a 63L) > 0

let generic_shift shifter x a =
  if bad_shift_amount a then
    0L
  else
    shifter x (to_int a)

let shiftl =
  generic_shift shift_left

let lshiftr =
  generic_shift shift_right_logical

let ashiftr =
  generic_shift shift_right

(*** bitsets ***)

let bitsubset sub super =
  (logand sub super) = sub

(*** masks ***)

let bitmask s l =
  if (compare (add s l) 64L) >= 0 then
    shiftl minus_one s
  else
    shiftl (lshiftr minus_one (sub 64L l)) s

let rec low_one_bits int =
  if is_bit_set int 0 then
    logor (shift_left (low_one_bits (shift_right_logical int 1)) 1) one
  else
    zero

let both_low_one_bits arg1 arg2 =
  logand (low_one_bits arg1) (low_one_bits arg2)

let rec low_mask int =
  if int = zero then
    zero
  else
    logor (shift_left (low_mask (shift_right_logical int 1)) 1) one

let rec high_mask int =
  if int = zero then
    zero
  else
    logor (shift_right_logical (high_mask (shift_left int 1)) 1) (shift_left one 63)

let is_mask_mask m w =
  let submask =
    sub (shift_left one w) one
  in let rec check m =
      if m = zero then
	true
      else
	let low_mask = logand m submask
	in if (low_mask = zero) || (low_mask = submask) then
	    check (shift_right_logical m w)
	  else
	    false
  in check m

(*** extracting/inserting ***)

let extract_bits x s l =
  logand (lshiftr x s) (bitmask 0L l)

let insert_bits x y s l =
  logor (logand x (lognot (bitmask s l)))
    (shiftl (logand y (bitmask 0L l)) s)

(*** parity ***)

let rec int_parity int =
  if int = 0L then 0L
  else logxor (logand int 1L) (int_parity (shift_right_logical int 1))

(*** comparisons ***)

let less_u arg1 arg2 =
  let result = compare (shift_right_logical arg1 32) (shift_right_logical arg2 32)
  in if result <> 0 then result < 0
  else (compare (logand arg1 0xffffffffL) (logand arg2 0xffffffffL)) < 0

let less_s arg1 arg2 =
  (compare arg1 arg2) < 0
