(*
 * machine.ml
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

exception Not_supported_in_dummy_machine

type machine =
    { addr_width : int ;
      register_width : register -> int }

type machine_insn =
    { machine_insn_name : string ;
      insn_stmt : stmt ;
      explore_fields : (string * int64 * int64) list }

let machine_ppc =
  { addr_width = 4 ;
    register_width = (fun _ -> 4) }

let dummy_machine =
  { addr_width = 8 ;
    register_width = (fun _ -> raise Not_supported_in_dummy_machine) }

let find_insn insns name =
  find (fun i -> i.machine_insn_name = name) insns
