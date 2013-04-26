open Xenlight
open Xenops_utils

module D = Debug.Make(struct let name = "libxl_events" end)
open D

module E = Async(
	struct
		type osevent_user = int
		type event_user = string
		type async_user = Condition.t
	end)
open E

(* event callbacks *)

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

(* async callbacks *)

let m = Mutex.create ()
let fired = ref None

let async f =
	debug "ASYNC call";
	let c = Condition.create () in
	E.async f c;
	debug "ASYNC call returned";
	Mutex.lock m;
	if !fired = None then
		Condition.wait c m;
	let result = match !fired with Some x -> x | None -> failwith "help!" in
	fired := None;
	Mutex.unlock m;
	result

let async_callback result c =
	debug "ASYNC callback: result=%d" result;
	Mutex.lock m;
	fired := Some result;
	Condition.signal c;
	Mutex.unlock m;
	()

(* event registration and main loop *)

let fds = ref []
let timeout = ref None
let (interrupt_in, interrupt_out) = Unix.pipe ()
let timeout_m = Mutex.create ()

let event_loop_start ctx =
	debug "Starting event loop";
	while true do
		let fds_in = !fds in
		let fds_in' = List.map (fun (a, b, c) -> a, b) fds_in in
		let now = int_of_float (Unix.gettimeofday () *. 1000.) in
		let timeout' = match !timeout with
			| None -> -1
			| Some (s, us, _) when s = 0 && us = 0 -> 0
			| Some (s, us, _) -> (s * 1000 + us / 1000) - now
		in
		(* the following list match is safe: it will never be and empty list, because we
		 * always give at least one fd to poll (the interrupt fd). *)
		debug "EVENTS: calling poll with timeout value %d" timeout';
		let (interrupt :: fds_out), rc = Poll.poll ((interrupt_in, [POLLIN]) :: fds_in') timeout' in
		if rc < 0 then
			error "EVENTS: poll error %d" rc
		else if rc = 0 then begin
			debug "EVENTS: timeout occurred";
			Opt.iter (fun (_, _, for_libxl) -> osevent_occurred_timeout ctx for_libxl) !timeout;
			Mutex.execute timeout_m (fun () -> timeout := None)
		end else begin
			(* callbacks into libxl *)
			List.iter2 (fun (fd, events, for_libxl) revents ->
				if revents <> [] then begin
					debug "EVENTS: fd event occurred";
					osevent_occurred_fd ctx for_libxl fd events revents
				end
			) fds_in fds_out;
			(* if the poll was interrupted by an fd update, clear the signal *)
			if interrupt <> [] then begin
				debug "EVENTS: poll interrupted for update";
				let buf = String.create 1 in
				ignore (Unix.read interrupt_in buf 0 1)
			end
		end
	done

let event_loop_init ctx =
	let fd_register user fd events for_libxl =
		debug "EVENTS: registering fd for %d" user;
		fds := (fd, events, for_libxl) :: !fds;
		ignore (Unix.write interrupt_out "r" 0 1)
	in
	let fd_modify user fd events =
		debug "EVENTS: modifying fd for %d" user;
		let rec replace = function
			| [] -> []
			| (fd', _, for_libxl) :: tl when fd' = fd -> (fd, events, for_libxl) :: tl
			| hd :: tl -> hd :: replace tl
		in
		fds := replace !fds;
		ignore (Unix.write interrupt_out "m" 0 1)
	in
	let fd_deregister user fd =
		debug "EVENTS: deregistering fd for %d" user;
		fds := List.filter (fun (fd', _, _) -> fd <> fd') !fds;
		ignore (Unix.write interrupt_out "d" 0 1)
	in
	let timeout_register user s us for_libxl =
		debug "EVENTS: registering timeout for %d (%ds, %dus)" user s us;
		Mutex.execute timeout_m (fun () -> timeout := Some (s, us, for_libxl));
		ignore (Unix.write interrupt_out "t" 0 1);
		()
	in
	let timeout_modify user =
		debug "EVENTS: modifying timeout for %d" user;
		match !timeout with
		| Some (_, _, for_libxl) ->
			Mutex.execute timeout_m (fun () -> timeout := Some (0, 0, for_libxl));
			ignore (Unix.write interrupt_out "t" 0 1);
			()
		| None -> ()
	in
	debug "Registering event hooks";
	let _ = osevent_register_hooks ctx ~user:666 ~fd_register ~fd_modify ~fd_deregister ~timeout_register ~timeout_modify in
	let _ = event_register_callbacks ctx ~user:"xenopsd-event" ~event_occurs_callback ~event_disaster_callback in
	let _ = async_register_callback ~async_callback in
	Thread.create (fun () -> event_loop_start ctx) ()

