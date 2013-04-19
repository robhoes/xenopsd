open Xenlight

let xl_async_callback wakener result =
	Lwt.wakeup wakener result

let xl_event_occurs_callback user event =
	let open Event in
	let ty = match event.ty with
		| Domain_shutdown _ -> "domain shutdown"
		| Domain_death -> "domain death"
		| Disk_eject _ -> "disk eject"
		| Operation_complete _ -> "operation complete"
		| Domain_create_console_available -> "domain create console available"
	in
	print_endline ("event occurred: " ^ ty ^ ", user " ^ user)

let xl_event_disaster_callback user event_type msg errnoval =
	print_endline ("event disaster: " ^ msg ^ ", user " ^ user)

let device_nic_add_async ctx nic domid =
	let t, u = Lwt.wait () in
	Device_nic.add ctx ~async:u nic domid;
	t

let event_loop ctx =
	let fds = ref [] in
	let (interrupt_in, interrupt_out) = Unix.pipe () in
	let fd_register user fd events for_libxl =
		fds := (fd, events, for_libxl) :: !fds;
		Unix.write interrupt_out "r" 0 1
	in
	let fd_modify user fd events =
		let rec replace = function
			| [] -> []
			| (fd', _, for_libxl) :: tl when fd' = fd -> (fd, events, for_libxl) :: tl
			| hd :: tl -> hd :: replace tl
		in
		fds := replace !fds;
		Unix.write interrupt_out "m" 0 1
	in
	let fd_deregister user fd =
		fds := List.filter (fun fd' -> fd <> fd') !fds;
		Unix.write interrupt_out "d" 0 1
	in
	Callback.register "xl_async_callback" xl_async_callback;
	Callback.register "xl_event_occurs_callback" xl_event_occurs_callback;
	Callback.register "xl_event_disaster_callback" xl_event_disaster_callback;
	Callback.register "fd_register" fd_register;
	Callback.register "fd_modify" fd_modify;
	Callback.register "fd_deregister" fd_deregister;
	let _ = osevent_register_hooks ctx "userob" in (* the last parameter comes back into the "user" param above *)
	let _ = event_register_callbacks ctx "userob2" in
	while true do
		let fds_in = (interrupt_in, [POLLIN]) :: List.map (fun (a, b, c) -> a, b) !fds in
		let fds_out = poll fds_in in
		(* callbacks into libxl *)
		if List.tl fds_out <> [] then
			List.iter2 (fun (fd, events, for_libxl) revents -> osevent_occurred_fd ctx for_libxl fd events revents) !fds (List.tl fds_out);
		(* if the poll was interrupted by an fd update, clear the signal *)
		if List.hd fds_out <> [] then begin
			let buf = String.create 1 in
			ignore (Unix.read interrupt_in buf 0 1)
		end
	done

let event_loop_lwt ctx =
	let make_watchers fd events for_libxl =
		let watchers = List.flatten (
			List.map (function
				| POLLIN ->
					Lwt_engine.on_readable fd (fun _ ->
						osevent_occurred_fd ctx for_libxl fd [POLLIN] [POLLIN]
					) :: []
				| POLLOUT ->
					Lwt_engine.on_writable fd (fun _ ->
						osevent_occurred_fd ctx for_libxl fd [POLLOUT] [POLLOUT]
					) :: []
				| _ -> []
			) events
		) in
		fd, watchers, for_libxl
	in

	let watch = ref [] in
	let fd_register user fd events for_libxl =
		let watchers = make_watchers fd events for_libxl in
		watch := watchers :: !watch
	in
	let fd_modify user fd events =
		let rec replace = function
			| [] -> []
			| (fd', ev, for_libxl) :: tl when fd' = fd ->
				List.iter Lwt_engine.stop_event ev;
				let watchers = make_watchers fd events for_libxl in
				watchers :: tl
			| hd :: tl -> hd :: replace tl
		in
		watch := replace !watch
	in
	let fd_deregister user fd =
		let rec remove = function
			| [] -> []
			| (fd', ev, for_libxl) :: tl when fd' = fd ->
				List.iter Lwt_engine.stop_event ev;
				tl
			| hd :: tl -> hd :: remove tl
		in
		watch := remove !watch
	in

	Callback.register "xl_async_callback" xl_async_callback;
	Callback.register "xl_event_occurs_callback" xl_event_occurs_callback;
	Callback.register "xl_event_disaster_callback" xl_event_disaster_callback;
	Callback.register "fd_register" fd_register;
	Callback.register "fd_modify" fd_modify;
	Callback.register "fd_deregister" fd_deregister;
	let _ = osevent_register_hooks ctx "userob" in (* the last parameter comes back into the "user" param above *)
	let _ = event_register_callbacks ctx "userob2" in
	()
