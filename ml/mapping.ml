(*
 * mapping.ml
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

open Expr
open Machine

exception Not_supported_in_dummy_mapping

type mapping =
    { source_machine : machine ;
      needed_target_width : register -> int ;
      register_known : register -> int64 ;
      register_bits : register -> int64 }

let mapping_needed_target_width register = 8

let mapping_ppc_to_alpha =
  { source_machine = machine_ppc ;
    needed_target_width = (fun _ -> 8) ;
    register_known = (fun _ -> zero) ;
    register_bits = (fun _ -> zero) }

let dummy_mapping =
  { source_machine = dummy_machine ;
    needed_target_width = (fun _ -> raise Not_supported_in_dummy_mapping) ;
    register_known = (fun _ -> raise Not_supported_in_dummy_mapping) ;
    register_bits = (fun _ -> raise Not_supported_in_dummy_mapping) }
