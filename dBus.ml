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
 * Dbus binding
 *)

exception Error of string * string

type bus
type message
type pending_call
type watch

type add_watch_fn = watch -> bool
type rm_watch_fn = watch -> unit
type toggle_watch_fn = watch -> unit
type watch_fns = add_watch_fn * rm_watch_fn * (toggle_watch_fn option)

type ty_sig =
	| SigByte
	| SigBool
	| SigInt16
	| SigUInt16
	| SigInt32
	| SigUInt32
	| SigInt64
	| SigUInt64
	| SigDouble
	| SigString
	| SigObjectPath
	| SigVariant
	| SigArray of ty_sig
	| SigStruct of ty_sig list
	| SigDict of ty_sig * ty_sig

type ty_array =
	| Unknowns
	| Bytes of char list
	| Bools of bool list
	| Int16s of int list
	| UInt16s of int list
	| Int32s of int32 list
	| UInt32s of int32 list
	| Int64s of int64 list
	| UInt64s of int64 list
	| Doubles of float list
	| Strings of string list
	| ObjectPaths of string list
	| Structs of ty_sig list * (ty list list)
	| Variants of ty list
	| Dicts of (ty_sig * ty_sig) * ((ty * ty) list)
and ty =
	| Unknown
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
	| ObjectPath of string
	| Array of ty_array
	| Struct of ty list
	| Variant of ty

let rec string_of_ty_array ty =
	match ty with
	| Unknowns -> []
	| Bytes cs -> List.map (fun x -> Printf.sprintf "%C" x) cs
	| Bools bs -> List.map (fun x -> Printf.sprintf "%b" x) bs
	| Int16s is -> List.map (fun x -> Printf.sprintf "%d" x) is
	| UInt16s is -> List.map (fun x -> Printf.sprintf "%d" x) is
	| Int32s is -> List.map (fun x -> Printf.sprintf "%ld" x) is
	| UInt32s is -> List.map (fun x -> Printf.sprintf "%ld" x) is
	| Int64s is -> List.map (fun x -> Printf.sprintf "%Ld" x) is
	| UInt64s is -> List.map (fun x -> Printf.sprintf "%Ld" x) is
	| Doubles fs -> List.map (fun x -> Printf.sprintf "%g" x) fs
	| Strings ss -> List.map (fun x -> Printf.sprintf "%S" x) ss
	| ObjectPaths ss -> List.map (fun x -> Printf.sprintf "%S" x) ss
	| Structs (ssig, ss) -> []
	| Variants (vs) -> List.map string_of_ty vs
	| Dicts ((ksig, vsig), ds) -> List.map (fun (k, v) -> Printf.sprintf "%s: %s" (string_of_ty k) (string_of_ty v)) ds
and string_of_ty ty =
	match ty with
	| Unknown      -> "Unknown"
	| Byte c       -> Printf.sprintf "Byte(%C)" c
	| Bool b       -> Printf.sprintf "Bool(%b)" b
	| Int16 i      -> Printf.sprintf "Int16(%d)" i
	| UInt16 i     -> Printf.sprintf "UInt16(%d)" i
	| Int32 i      -> Printf.sprintf "Int32(%ld)" i
	| UInt32 i     -> Printf.sprintf "UInt32(%ld)" i
	| Int64 i      -> Printf.sprintf "Int64(%Ld)" i
	| UInt64 i     -> Printf.sprintf "UInt64(%Ld)" i
	| Double d     -> Printf.sprintf "Double(%g)" d
	| String s     -> Printf.sprintf "String(%S)" s
	| ObjectPath s -> Printf.sprintf "ObjectPath(%S)" s
	| Array Unknowns -> Printf.sprintf "Array[...]"
	| Array ty     -> Printf.sprintf "Array[%s]" (String.concat ", " (string_of_ty_array ty))
	| Struct tys   -> Printf.sprintf "Struct{%s}" (String.concat ", " (List.map string_of_ty tys))
	| Variant ty   -> Printf.sprintf "Variant{%s}" (string_of_ty ty)

(******************** BUS **********************)
module Bus = struct
type ty = Session | System | Starter
type flags = Replace_existing

external get : ty -> bus = "stub_dbus_bus_get"
external get_private : ty -> bus = "stub_dbus_bus_get_private"
external register : bus -> unit = "stub_dbus_bus_register"
external set_unique_name : bus -> string -> bool
                         = "stub_dbus_bus_set_unique_name"
external get_unique_name : bus -> string = "stub_dbus_bus_get_unique_name"
external request_name : bus -> string -> int -> unit
                      = "stub_dbus_bus_request_name"
external release_name : bus -> string -> unit
                      = "stub_dbus_bus_release_name"
external has_owner : bus -> string -> bool = "stub_dbus_bus_has_owner"
external add_match : bus -> string -> bool -> unit = "stub_dbus_bus_add_match"
external remove_match : bus -> string -> bool -> unit = "stub_dbus_bus_remove_match"

end

(****************** MESSAGE ********************)
module Message = struct
type message_type = Invalid | Method_call | Method_return | Error | Signal

let string_of_message_ty ty =
	match ty with
	| Invalid       -> "invalid"
	| Method_call   -> "method_call"
	| Method_return -> "method_return"
	| Error         -> "error"
	| Signal        -> "signal"

external create : message_type -> message = "stub_dbus_message_create"
(** [create message_type] create a new empty message with a specific type.
   recommended not to use this call but use new_* calls instead that prefil
   all the required field too. *)

external new_method_call : string -> string -> string -> string -> message
                         = "stub_dbus_message_new_method_call"
(** [new_method_call destination path interface method] create a new method message *)

external new_method_return : message -> message =
                           "stub_dbus_message_new_method_return"
(** [new_method_return message] create a new method return message from a method message *)

external new_signal : string -> string -> string -> message
                    = "stub_dbus_message_new_signal"
(** [new_signal path interface method] create a new signal message *)

external new_error : message -> string -> string -> message
                    = "stub_dbus_message_new_error"
(** [new_error original_message error_name error_message] create a new error message
   from another message *)

external append : message -> ty list -> unit = "stub_dbus_message_append"
(** [append message parameters] appends dbus parameters to the message *)

external get_rev : message -> ty list = "stub_dbus_message_get"

let get message = List.rev (get_rev message)
(** [get message] returns all parameters associated with the message *)

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
type dispatch_status = Data_remains | Complete | Need_memory

external send : bus -> message -> int32
              = "stub_dbus_connection_send"
external send_with_reply : bus -> message -> int -> pending_call
              = "stub_dbus_connection_send_with_reply"
external send_with_reply_and_block : bus -> message -> int -> message
              = "stub_dbus_connection_send_with_reply_and_block"
external add_filter : bus -> (bus -> message -> bool) -> unit
                    = "stub_dbus_connection_add_filter"
external flush : bus -> unit = "stub_dbus_connection_flush"

external read_write : bus -> int -> bool = "stub_dbus_connection_read_write"
(** [read_write bus timeout_millisecond] will block until it can read or write.
   return value indicates whether connection is still open *)

external read_write_dispatch : bus -> int -> bool
                             = "stub_dbus_connection_read_write_dispatch"
(** [read_write_dispatch bus timeout_millisecond] will block until it can read or write.
   return value indicates whether disconnect message has been processed *)

external pop_message : bus -> message option
                     = "stub_dbus_connection_pop_message"

external get_dispatch_status : bus -> dispatch_status
	= "stub_dbus_connection_get_dispatch_status"
(** [get_dispatch_status bus] gets the current state of the incoming message queue. *)
external dispatch : bus -> dispatch_status
	= "stub_dbus_connection_dispatch"
(** [dispatch bus] Processes any incoming data. *)

external get_fd : bus -> Unix.file_descr = "stub_dbus_connection_get_fd"

external set_watch_functions : bus -> watch_fns -> unit
	= "stub_dbus_connection_set_watch_functions"
(** [set_watch_function bus addfn rmfn togglefn] set the watch functions for the connection.
 *)

external get_max_message_size : bus -> int = "stub_dbus_connection_get_max_message_size"
external set_max_message_size : bus -> int -> unit = "stub_dbus_connection_set_max_message_size"

external get_max_received_size : bus -> int = "stub_dbus_connection_get_max_received_size"
external set_max_received_size : bus -> int -> unit = "stub_dbus_connection_get_max_received_size"

external get_outgoing_size : bus -> int = "stub_dbus_connection_get_outgoing_size"

external set_allow_anonymous : bus -> bool -> unit = "stub_dbus_connection_set_allow_anonymous"

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

module Watch = struct

type flags = Readable | Writable

external get_unix_fd : watch -> Unix.file_descr = "stub_dbus_watch_get_unix_fd"
external get_enabled : watch -> bool = "stub_dbus_watch_get_enabled"
external get_flags : watch -> flags list = "stub_dbus_watch_get_flags"
external handle : watch -> flags list -> unit = "stub_dbus_watch_handle"

end

let _ = Callback.register_exception "dbus.error" (Error ("register_callback", "register_callback"))
