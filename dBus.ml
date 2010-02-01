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
	| UInt16 of int
	| Int32 of int32
	| UInt32 of int32
	| Int64 of int64
	| UInt64 of int64
	| Double of float
	| String of string

(******************* ERROR *********************)
module Error = struct
external init : unit -> error = "stub_dbus_error_init"
external is_set : error -> bool = "stub_dbus_error_is_set"
external has_name : error -> string -> bool = "stub_dbus_error_has_name"
end

(******************** BUS **********************)
module Bus = struct
type ty = Session | System | Starter
type flags = Replace_existing

external get : ty -> error -> bus = "stub_dbus_bus_get"
external get_private : ty -> error -> bus = "stub_dbus_bus_get_private"
external register : bus -> error -> bool = "stub_dbus_bus_register"
external set_unique_name : bus -> string -> bool
                         = "stub_dbus_bus_set_unique_name"
external get_unique_name : bus -> string = "stub_dbus_bus_get_unique_name"
external request_name : bus -> string -> int -> error -> unit
                      = "stub_dbus_bus_request_name"
external release_name : bus -> string -> error -> unit
                      = "stub_dbus_bus_release_name"
external has_owner : bus -> string -> error -> bool = "stub_dbus_bus_has_owner"
external add_match : bus -> string -> error -> unit = "stub_dbus_bus_add_match"
external remove_match : bus -> string -> error -> unit
                      = "stub_dbus_bus_remove_match"

end

(****************** MESSAGE ********************)
module Message = struct
type message_type = Invalid | Method_call | Method_return | Error | Signal

external create : message_type -> message = "stub_dbus_message_create"
external new_method_call : string -> string -> string -> string -> message
                         = "stub_dbus_message_new_method_call"
external new_method_return : message -> message =
                           "stub_dbus_message_new_method_return"
external new_signal : string -> string -> string -> message
                    = "stub_dbus_message_new_signal"
external new_error : message -> string -> string -> message
                    = "stub_dbus_message_new_error"

external append : message -> ty list -> unit = "stub_dbus_message_append"
external get_rev : message -> ty list = "stub_dbus_message_get"
let get message = List.rev (get_rev message)


external set_path : message -> string -> unit = "stub_dbus_message_set_path"
external set_interface : message -> string -> unit = "stub_dbus_message_set_interface"
external set_member : message -> string -> unit = "stub_dbus_message_set_member"
external set_error_name : message -> string -> unit = "stub_dbus_message_set_error_name"
external set_destination : message -> string -> unit = "stub_dbus_message_set_destination"
external set_sender : message -> string -> unit = "stub_dbus_message_set_sender"
external set_reply_serial : message -> int32 -> unit
                          = "stub_dbus_message_set_reply_serial"
external set_auto_start : message -> bool -> unit
                        = "stub_dbus_message_set_auto_start"


external has_path : message -> string -> bool = "stub_dbus_message_has_path"
external has_interface : message -> string -> bool = "stub_dbus_message_has_interface"
external has_member : message -> string -> bool = "stub_dbus_message_has_member"
external has_destination : message -> string -> bool = "stub_dbus_message_has_destination"
external has_sender : message -> string -> bool = "stub_dbus_message_has_sender"
external has_signature : message -> string -> bool = "stub_dbus_message_has_signature"


external get_type : message -> message_type = "stub_dbus_message_get_type"
external get_path : message -> string option = "stub_dbus_message_get_path"
external get_interface : message -> string option
                       = "stub_dbus_message_get_interface"
external get_member : message -> string option
                    = "stub_dbus_message_get_member"
external get_error_name : message -> string option
                        = "stub_dbus_message_get_error_name"
external get_destination : message -> string option
                         = "stub_dbus_message_get_destination"
external get_sender : message -> string option
                    = "stub_dbus_message_get_sender"
external get_signature : message -> string option
                       = "stub_dbus_message_get_signature"
external get_serial : message -> int32 = "stub_dbus_message_get_serial"
external get_reply_serial : message -> int32
                          = "stub_dbus_message_get_reply_serial"
external get_auto_start : message -> bool
                        = "stub_dbus_message_get_auto_start"

external is_signal : message -> string -> string -> bool
                   = "stub_dbus_message_is_signal"
external is_method_call : message -> string -> string -> bool
                        = "stub_dbus_message_is_method_call"
external is_error : message -> string -> bool = "stub_dbus_message_is_error"
end

(**************** CONNECTION *******************)
module Connection = struct
external send : bus -> message -> int32
              = "stub_dbus_connection_send"
external send_with_reply : bus -> message -> int -> pending_call
              = "stub_dbus_connection_send_with_reply"
external send_with_reply_and_block : bus -> message -> int -> error -> message
              = "stub_dbus_connection_send_with_reply_and_block"
external add_filter : bus -> (bus -> message -> bool) -> unit
                    = "stub_dbus_connection_add_filter"
external flush : bus -> unit = "stub_dbus_connection_flush"
external read_write : bus -> int -> bool = "stub_dbus_connection_read_write"
external read_write_dispatch : bus -> int -> bool
                             = "stub_dbus_connection_read_write_dispatch"
external pop_message : bus -> message option
                     = "stub_dbus_connection_pop_message"
external get_fd : bus -> Unix.file_descr = "stub_dbus_connection_get_fd"
end

(***************** PENDING ********************)
module PendingCall = struct
external block : pending_call -> unit = "stub_dbus_pending_call_block"
external cancel : pending_call -> unit = "stub_dbus_pending_call_cancel"
external get_completed : pending_call -> bool
                       = "stub_dbus_pending_call_get_completed"
external steal_reply : pending_call -> message
                     = "stub_dbus_pending_call_steal_reply"
end
