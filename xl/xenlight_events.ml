open Xenlight
open Xenops_utils

module D = Debug.Make(struct let name = "libxl_events" end)
open D

let event_occurs_callback user event =
	let open Event in
	let ty = match event.ty with
		| Domain_shutdown _ -> "domain shutdown"
		| Domain_death -> "domain death"
		| Disk_eject _ -> "disk eject"
		| Operation_complete _ -> "operation complete"
		| Domain_create_console_available -> "domain create console available"
	in
	debug "EVENT occurred: %s, callback user %s, event user %Ld" ty user event.for_user

let event_disaster_callback user event_type msg errnoval =
	debug "EVENT disaster: %s, user %s" msg user

let fds = ref []
let timeout = ref None
let (interrupt_in, interrupt_out) = Unix.pipe ()

let event_loop_init ctx =
	let fd_register user fd events for_libxl =
		debug "EVENTS: registering fd for %s" user;
		fds := (fd, events, for_libxl) :: !fds;
		ignore (Unix.write interrupt_out "r" 0 1)
	in
	let fd_modify user fd events =
		debug "EVENTS: modifying fd for %s" user;
		let rec replace = function
			| [] -> []
			| (fd', _, for_libxl) :: tl when fd' = fd -> (fd, events, for_libxl) :: tl
			| hd :: tl -> hd :: replace tl
		in
		fds := replace !fds;
		ignore (Unix.write interrupt_out "m" 0 1)
	in
	let fd_deregister user fd =
		debug "EVENTS: deregistering fd for %s" user;
		fds := List.filter (fun (fd', _, _) -> fd <> fd') !fds;
		ignore (Unix.write interrupt_out "d" 0 1)
	in
	let timeout_register user ms for_libxl =
		debug "EVENTS: registering timeout for %s (%dms)" user ms;
		timeout := Some (ms, for_libxl);
		ignore (Unix.write interrupt_out "t" 0 1);
		()
	in
	let timeout_modify user ms =
		debug "EVENTS: modifying timeout for %s (%dms)" user ms;
		match !timeout with
		| Some (_, for_libxl) ->
			timeout := Some (0, for_libxl);
			ignore (Unix.write interrupt_out "t" 0 1);
			()
		| None -> ()
	in
	debug "Registering event hooks";
	let _ = osevent_register_hooks ctx ~user:"userob" ~fd_register ~fd_modify ~fd_deregister ~timeout_register ~timeout_modify in
	let _ = event_register_callbacks ctx ~user:"userob2" ~event_occurs_callback ~event_disaster_callback in
	()

let event_loop_start ctx =
	debug "Starting event loop";
	while true do
		let fds_in = (interrupt_in, [POLLIN]) :: List.map (fun (a, b, c) -> a, b) !fds in
		let timeout' = match !timeout with None -> -1 | Some (t, _) -> t in
		(* the following list match is safe: it will never be and empty list, because we
		 * always give at least one fd to poll (the interrupt fd). *)
		let (interrupt :: fds_out), rs = Poll.poll fds_in timeout' in
		if rs < 0 then
			error "EVENTS: poll error %d" rs
		else if rs = 0 then begin
			debug "EVENTS: timeout occurred";
			Opt.iter (fun (_, for_libxl) -> osevent_occurred_timeout ctx for_libxl) !timeout;
			timeout := None
		end else begin
			(* callbacks into libxl *)
			List.iter2 (fun (fd, events, for_libxl) revents ->
				if revents <> [] then begin
					debug "EVENTS: fd event occurred";
					osevent_occurred_fd ctx for_libxl fd events revents
				end
			) !fds (List.tl fds_out);
			(* if the poll was interrupted by an fd update, clear the signal *)
			if interrupt <> [] then begin
				let buf = String.create 1 in
				ignore (Unix.read interrupt_in buf 0 1)
			end
		end
	done

