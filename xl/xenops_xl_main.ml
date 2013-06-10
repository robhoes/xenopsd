(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Start the program with the xenlight backend *)
let _ =
	Xenops_interface.queue_name := !Xenops_interface.queue_name ^ ".xenlight";
	Xenops_utils.set_root "xenopsd/xenlight";
	Xenopsd.main
		~specific_essential_paths:Xl_path.essentials
		~specific_nonessential_paths:Xl_path.nonessentials
		(module Xenops_server_xenlight: Xenops_server_plugin.S)

