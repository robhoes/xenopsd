(* event loop helper wrapping the poll syscall *)
exception Poll_error of string
external poll : (Unix.file_descr * Xenlight.event list) list -> int -> (Xenlight.event list list * int) = "stub_poll"

