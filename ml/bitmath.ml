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

(*** masks ***)

let bitmask s l =
  if (compare (add s l) 64L) >= 0 then
    shiftl minus_one s
  else
    shiftl (lshiftr minus_one (sub 64L l)) s

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
