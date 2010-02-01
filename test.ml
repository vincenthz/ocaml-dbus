(*
 *	Copyright (C) 2006-2009 Vincent Hanquez <vincent@snarc.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Dbus example
 *)

open Printf

let client bus =
	let msg = DBus.Message.new_signal "/test/signal/Object"
	                                  "test.signal.Type" "Test" in
	DBus.Message.append msg [ (DBus.String "Ping!!"); (DBus.Bool false) ];
	let serial = DBus.Connection.send bus msg in
	printf "client serial: %ld\n" serial;
	DBus.Connection.flush bus

let server bus =
	let match_s = "type='signal',interface='test.signal.Type'" in
	DBus.Bus.add_match bus match_s false;
	DBus.Connection.flush bus;
	DBus.Connection.add_filter bus (fun bus msg -> true);

	while true
	do
		DBus.Connection.read_write bus 0;
		let msg = DBus.Connection.pop_message bus in
		match msg with
		| None -> Unix.sleep 1; ()
		| Some msg ->
			if DBus.Message.is_signal msg "test.signal.Type" "Test" then (
				let params = DBus.Message.get msg in
				let show p =
					match p with
					| DBus.String s -> printf "S: %s\n%!" s;
					| DBus.Int32 i -> printf "I: %ld\n%!" i;
					| DBus.Bool b -> printf "B: %b\n%!" b;
					| _ -> printf "other type\n";
					in

				List.iter show params
			) else if DBus.Message.is_method_call msg
			                       "test.method.Type" "Method" then (
				printf "method call\n%!";
			) else
				printf "other call\n%!"
	done

let _ =
	if (Array.length Sys.argv) < 2 then (
		eprintf "usage: test [server|client]\n";
		exit 1
	);

	let bus = DBus.Bus.get DBus.Bus.System in

	match Sys.argv.(1) with
	| "server" -> server bus
	| "client" -> client bus
	| _        -> eprintf "unknown command\n";
