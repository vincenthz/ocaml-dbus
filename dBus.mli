(*
 *	Copyright (C) 2006 Vincent Hanquez <vincent@snarc.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Dbus binding
 *)

type error
type bus
type message
type pending_call
type ty =
	| Byte of char
	| Bool of bool
	| Int16 of int
	| Int32 of int32
	| Int64 of int64
	| Double of float
	| String of string

module Error :
sig
	val init : unit -> error
	val is_set : error -> bool
	val has_name : error -> string -> bool
end

module Bus :
sig
	type ty = Session | System | Starter
	type flags = Replace_existing

	val get : ty -> error -> bus
	val get_private : ty -> error -> bus
	val register : bus -> error -> bool
	val set_unique_name : bus -> string -> bool
	val get_unique_name : bus -> string
	val request_name : bus -> string -> int -> error -> unit
	val release_name : bus -> string -> error -> unit
	val has_owner : bus -> string -> error -> bool
	val add_match : bus -> string -> error -> unit
	val remove_match : bus -> string -> error -> unit
end

module Message :
sig
	type message_type =
		| Invalid
		| Method_call
		| Method_return
		| Error
		| Signal
	val create : message_type -> message
	val new_method_call : string -> string -> string -> string -> message
	val new_method_return : message -> message
	val new_signal : string -> string -> string -> message
	val new_error : message -> string -> string -> message
	val append : message -> ty list -> unit
	val get_rev : message -> ty list
	val get : message -> ty list
	val set_path : message -> string -> unit
	val set_interface : message -> string -> unit
	val set_member : message -> string -> unit
	val set_error_name : message -> string -> unit
	val set_destination : message -> string -> unit
	val set_sender : message -> string -> unit
	val set_reply_serial : message -> int32 -> unit
	val set_auto_start : message -> bool -> unit
	val has_path : message -> string -> bool
	val has_interface : message -> string -> bool
	val has_member : message -> string -> bool
	val has_destination : message -> string -> bool
	val has_sender : message -> string -> bool
	val has_signature : message -> string -> bool
	val get_type : message -> message_type
	val get_path : message -> string
	val get_interface : message -> string
	val get_member : message -> string
	val get_error_name : message -> string
	val get_destination : message -> string
	val get_sender : message -> string
	val get_signature : message -> string
	val get_serial : message -> int32
	val get_reply_serial : message -> int32
	val get_auto_start : message -> bool
	val is_signal : message -> string -> string -> bool
	val is_method_call : message -> string -> string -> bool
	val is_error : message -> string -> bool
end

module Connection :
sig
	val send : bus -> message -> int32
	val send_with_reply : bus -> message -> int -> pending_call
	val add_filter : bus -> (bus -> message -> bool) -> unit
	val flush : bus -> unit
	val read_write : bus -> int -> unit
	val pop_message : bus -> message option
	val get_fd : bus -> Unix.file_descr
end

module PendingCall :
sig
	val block : pending_call -> unit
	val cancel : pending_call -> unit
	val get_completed : pending_call -> bool
	val steal_reply : pending_call -> message
end
