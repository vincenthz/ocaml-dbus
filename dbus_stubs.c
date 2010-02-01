/*
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
 */

#include <string.h>
#include <dbus/dbus.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>

#define SIZEOF_FINALPTR		(2 * sizeof (void *))

static int __messagetype_table[] = {
	DBUS_MESSAGE_TYPE_INVALID,
	DBUS_MESSAGE_TYPE_METHOD_CALL,
	DBUS_MESSAGE_TYPE_METHOD_RETURN,
	DBUS_MESSAGE_TYPE_ERROR,
	DBUS_MESSAGE_TYPE_SIGNAL,
	-1
};

static int __bustype_table[] = {
	DBUS_BUS_SESSION, DBUS_BUS_SYSTEM, DBUS_BUS_STARTER, -1
};

static int __type_table[] = {
	DBUS_TYPE_BYTE, DBUS_TYPE_BOOLEAN,
	DBUS_TYPE_INT16, DBUS_TYPE_UINT16,
	DBUS_TYPE_INT32, DBUS_TYPE_UINT32,
	DBUS_TYPE_INT64, DBUS_TYPE_UINT64,
	DBUS_TYPE_DOUBLE, DBUS_TYPE_STRING,
	-1
};

static int find_index_equal(int searched_value, int *table)
{
	int i;

	for (i = 0; table[i] != -1; i++)
		if (table[i] == searched_value)
			return i;
	return -1;
}

#define DBusConnection_val(v)   ((DBusConnection *) Field(v, 1))
#define DBusMessage_val(v)      ((DBusMessage *) Field(v, 1))
#define DBusError_val(v)        ((DBusError *) Field(v, 1))
#define DBusPendingCall_val(v)	((DBusPendingCall *) Field(v, 1))

#define voidstar_alloc(o_con, c_con, final_fct)				\
	o_con = caml_alloc_final(SIZEOF_FINALPTR, final_fct,		\
	                         SIZEOF_FINALPTR, 10 * SIZEOF_FINALPTR);\
	Store_field(o_con, 1, (value) c_con);

void finalize_dbus_error(value v)
{
	dbus_error_free(DBusError_val(v));
	free(DBusError_val(v));
}

void finalize_dbus_connection(value v)
{
	dbus_connection_close(DBusConnection_val(v));
}

void finalize_dbus_message(value v)
{
	DBusMessage *msg = DBusMessage_val(v);
	dbus_message_unref(msg);
}

void finalize_dbus_pending_call(value v)
{
	dbus_pending_call_unref(DBusPendingCall_val(v));
}

/******************* ERROR *********************/
value stub_dbus_error_init(value unit)
{
	CAMLparam1(unit);
	CAMLlocal1(error);
	DBusError *c_error = malloc(sizeof(DBusError));
	if (!c_error)
		caml_raise_out_of_memory();
	dbus_error_init(c_error);

	voidstar_alloc(error, c_error, finalize_dbus_error);
	CAMLreturn(error);
}

value stub_dbus_error_is_set(value error)
{
	CAMLparam1(error);
	int ret;

	ret = dbus_error_is_set(DBusError_val(error));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_error_has_name(value error, value name)
{
	CAMLparam2(error, name);
	int ret;

	ret = dbus_error_has_name(DBusError_val(error), String_val(name));
	CAMLreturn(Val_bool(ret));
}

/******************** BUS **********************/
value stub_dbus_bus_get(value type, value error)
{
	CAMLparam2(type, error);
	CAMLlocal1(con);
	DBusConnection *c_con;

	c_con = dbus_bus_get(__bustype_table[Int_val(type)],
	                     DBusError_val(error));
	if (!c_con)
		failwith("dbus_bus_get");

	voidstar_alloc(con, c_con, finalize_dbus_connection);
	CAMLreturn(con);
}

value stub_dbus_bus_get_private(value type, value error)
{
	CAMLparam2(type, error);
	CAMLlocal1(con);
	DBusConnection *c_con;

	c_con = dbus_bus_get_private(__bustype_table[Int_val(type)],
	                             DBusError_val(error));
	if (!c_con)
		failwith("dbus_bus_get");

	voidstar_alloc(con, c_con, finalize_dbus_connection);
	CAMLreturn(con);
}

value stub_dbus_bus_register(value bus, value error)
{
	CAMLparam2(bus, error);
	int ret;

	ret = dbus_bus_register(DBusConnection_val(bus), DBusError_val(error));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_bus_set_unique_name(value bus, value name)
{
	CAMLparam2(bus, name);
	int ret;

	ret = dbus_bus_set_unique_name(DBusConnection_val(bus),
	                               String_val(name));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_bus_get_unique_name(value bus)
{
	CAMLparam1(bus);
	CAMLlocal1(ret);
	const char *s;

	s = dbus_bus_get_unique_name(DBusConnection_val(bus));
	/* XXX do we need to free s after use ? */
	ret = caml_copy_string(s);
	CAMLreturn(ret);
}

value stub_dbus_bus_request_name(value bus, value name,
                                 value flags, value error)
{
	CAMLparam4(bus, name, flags, error);

	dbus_bus_request_name(DBusConnection_val(bus), String_val(name),
	                      Int_val(flags), DBusError_val(error));
	CAMLreturn(Val_unit);
}

value stub_dbus_bus_release_name(value bus, value name, value error)
{
	CAMLparam3(bus, name, error);

	dbus_bus_release_name(DBusConnection_val(bus), String_val(name),
	                      DBusError_val(error));
	CAMLreturn(Val_unit);
}

value stub_dbus_bus_has_owner(value bus, value name, value error)
{
	CAMLparam3(bus, name, error);
	int ret;

	ret = dbus_bus_name_has_owner(DBusConnection_val(bus),
	                              String_val(name), DBusError_val(error));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_bus_add_match(value bus, value s, value error)
{
	CAMLparam3(bus, s, error);

	dbus_bus_add_match(DBusConnection_val(bus), String_val(s),
	                   DBusError_val(error));
	CAMLreturn(Val_unit);
}

value stub_dbus_bus_remove_match(value bus, value s, value error)
{
	CAMLparam3(bus, s, error);

	dbus_bus_remove_match(DBusConnection_val(bus), String_val(s),
	                      DBusError_val(error));
	CAMLreturn(Val_unit);
}

/**************** CONNECTION *******************/
value stub_dbus_connection_send(value bus, value message)
{
	CAMLparam2(bus, message);
	CAMLlocal1(serial);
	unsigned int c_serial;

	dbus_connection_send(DBusConnection_val(bus), DBusMessage_val(message),
	                     &c_serial);
	serial = caml_copy_int32(c_serial);
	CAMLreturn(serial);
}

value stub_dbus_connection_send_with_reply(value bus, value message,
                                           value timeout)
{
	CAMLparam3(bus, message, timeout);
	CAMLlocal1(pending);
	DBusPendingCall *c_pending;
	int ret;

	ret = dbus_connection_send_with_reply(DBusConnection_val(bus),
	                                      DBusMessage_val(message),
	                                      &c_pending, Int_val(timeout));
	if (!ret) {
		free(c_pending);
		caml_failwith("dbus_connection_send_with_reply");
	}

	voidstar_alloc(pending, c_pending, finalize_dbus_pending_call);
	CAMLreturn(pending);
}

value stub_dbus_connection_send_with_reply_and_block(value bus, value message,
                                                     value timeout,
                                                     value error)
{
	CAMLparam4(bus, message, timeout, error);
	CAMLlocal1(rmsg);
	DBusMessage *c_rmsg;

	c_rmsg = dbus_connection_send_with_reply_and_block(
	                                               DBusConnection_val(bus),
	                                               DBusMessage_val(message),
	                                               Int_val (timeout),
	                                               DBusError_val(error));
	if (!c_rmsg)
		caml_failwith("dbus_connection_send_with_reply_and_block");

	voidstar_alloc(rmsg, c_rmsg, finalize_dbus_message);
	CAMLreturn(rmsg);
}

DBusHandlerResult add_filter_callback(DBusConnection *connection,
                                      DBusMessage *message,
                                      void *userdata)
{
	CAMLparam0();
	CAMLlocal2(conn, msg);
	int ret;

	voidstar_alloc(conn, connection, finalize_dbus_connection);
	voidstar_alloc(msg, message, finalize_dbus_message);
	ret = Bool_val(caml_callback2(*((value *) userdata),
	                              conn, msg));

	CAMLreturn ((ret)
		? DBUS_HANDLER_RESULT_HANDLED
		: DBUS_HANDLER_RESULT_NOT_YET_HANDLED);
}

static void dbus_free_filter(void *_v)
{
	value *v = _v;
	caml_remove_global_root(v);
	free(v);
}

value stub_dbus_connection_add_filter(value bus, value callback)
{
	CAMLparam2(bus, callback);
	value *callbackp;

	callbackp = malloc(sizeof(value));
	if (!callbackp)
		caml_raise_out_of_memory();
	*callbackp = callback;
	caml_register_global_root(callbackp);
	dbus_connection_add_filter(DBusConnection_val(bus),
	                           add_filter_callback, callbackp,
	                           dbus_free_filter);

	CAMLreturn(Val_unit);
}

value stub_dbus_connection_flush(value bus)
{
	CAMLparam1(bus);
	dbus_connection_flush(DBusConnection_val(bus));
	CAMLreturn(Val_unit);
}

value stub_dbus_connection_read_write(value bus, value timeout)
{
	CAMLparam2(bus, timeout);
	dbus_bool_t b;
	b = dbus_connection_read_write(DBusConnection_val(bus), Int_val(timeout));
	CAMLreturn(Val_bool(b));
}

value stub_dbus_connection_read_write_dispatch(value bus, value timeout)
{
	CAMLparam2(bus, timeout);
	dbus_bool_t b;
	b = dbus_connection_read_write_dispatch(DBusConnection_val(bus),
	                                        Int_val(timeout));
	CAMLreturn(Val_bool(b));
}

value stub_dbus_connection_pop_message(value bus)
{
	CAMLparam1(bus);
	CAMLlocal2(msg_opt, msg);
	DBusMessage *c_msg;

	msg_opt = Val_int(0); /* None */
	msg = Val_unit;

	c_msg = dbus_connection_pop_message(DBusConnection_val(bus));
	if (c_msg) {
		voidstar_alloc(msg, c_msg, finalize_dbus_message);
		msg_opt = caml_alloc_small(1, 0);
		Field(msg_opt, 0) = msg;
	}
	CAMLreturn(msg_opt);
}

value stub_dbus_connection_get_fd(value bus)
{
	CAMLparam1(bus);
	int fd, ret;

	ret = dbus_connection_get_unix_fd(DBusConnection_val(bus), &fd);
	if (ret == 0)
		caml_failwith("dbus_connection_get_fd");
	CAMLreturn(Val_int(fd));
}

/****************** MESSAGE ********************/
value stub_dbus_message_create(value message_type)
{
	CAMLparam1(message_type);
	CAMLlocal1(msg);
	DBusMessage *c_msg;
	int c_message_type;

	c_message_type = __messagetype_table[Int_val(message_type)];
	c_msg = dbus_message_new(c_message_type);
	if (!c_msg)
		caml_failwith("message_create");
	voidstar_alloc(msg, c_msg, finalize_dbus_message);
	CAMLreturn(msg);
}

value stub_dbus_message_new_method_call(value bus_name, value path,
                                        value interface, value method)
{
	CAMLparam4(bus_name, path, interface, method);
	CAMLlocal1(msg);
	DBusMessage *c_msg;

	c_msg = dbus_message_new_method_call(String_val(bus_name),
	                                     String_val(path),
	                                     String_val(interface),
	                                     String_val(method));
	if (!c_msg)
		caml_failwith("message_new_method_call");
	voidstar_alloc(msg, c_msg, finalize_dbus_message);
	CAMLreturn(msg);
}

value stub_dbus_message_new_method_return(value message)
{
	CAMLparam1(message);
	CAMLlocal1(new_message);
	DBusMessage *c_new_message;

	c_new_message = dbus_message_new_method_return(DBusMessage_val(message));
	if (!c_new_message)
		caml_failwith("message_new_method_call");
	voidstar_alloc(new_message, c_new_message, finalize_dbus_message);
	CAMLreturn(new_message);
}

value stub_dbus_message_new_signal(value path, value interface, value method)
{
	CAMLparam3(path, interface, method);
	CAMLlocal1(msg);
	DBusMessage *c_msg;

	c_msg = dbus_message_new_signal(String_val(path), String_val(interface),
	                                String_val(method));
	if (!c_msg)
		caml_failwith("message_new_signal");

	voidstar_alloc(msg, c_msg, finalize_dbus_message);
	CAMLreturn(msg);
}

value stub_dbus_message_new_error(value reply_to, value error_name,
                                  value error_message)
{
	CAMLparam3(reply_to, error_name, error_message);
	CAMLlocal1(msg);
	DBusMessage *c_msg;

	c_msg = dbus_message_new_error(DBusMessage_val(reply_to),
	                               String_val(error_name),
	                               String_val(error_message));
	if (!c_msg)
		caml_failwith("message_new_error");
	voidstar_alloc(msg, c_msg, finalize_dbus_message);
	CAMLreturn(msg);
}

value stub_dbus_message_get_type(value message)
{
	CAMLparam1(message);
	CAMLlocal1(ret);
	int c_type;

	c_type = dbus_message_get_type(DBusMessage_val(message));
	ret = Val_int(find_index_equal(c_type, __messagetype_table));
	CAMLreturn(ret);
}

#define MESSAGE_GET_ACCESSOR(type)				\
value stub_dbus_message_get_##type (value message)		\
{								\
	CAMLparam1(message);					\
	CAMLlocal2(v, vfield);					\
	const char *c_v;					\
	c_v = dbus_message_get_##type (DBusMessage_val(message)); \
	if (!c_v)						\
		CAMLreturn(Val_int(0));				\
	vfield = caml_copy_string(c_v);				\
	v = caml_alloc_small(1, 0);				\
	Field(v, 0) = vfield;					\
	CAMLreturn(v);						\
}								\

#define MESSAGE_SET_ACCESSOR(type)				\
value stub_dbus_message_set_##type (value message, value v)	\
{								\
	CAMLparam2(message, v);					\
	dbus_message_set_##type (DBusMessage_val(message),	\
	                         String_val(v));		\
	CAMLreturn(Val_unit);					\
}

#define MESSAGE_HAS_ACCESSOR(type)				\
value stub_dbus_message_has_##type (value message, value v)	\
{								\
	CAMLparam2(message, v);					\
	int ret;						\
	ret = dbus_message_has_##type (DBusMessage_val(message),\
	                               String_val(v));		\
	CAMLreturn(Val_bool(ret));				\
}

#define MESSAGE_ACCESSOR(type)					\
	MESSAGE_GET_ACCESSOR(type)				\
	MESSAGE_SET_ACCESSOR(type)				\
	MESSAGE_HAS_ACCESSOR(type)

#define MESSAGE_ACCESSOR_NOHAS(type)				\
	MESSAGE_GET_ACCESSOR(type)				\
	MESSAGE_SET_ACCESSOR(type)

MESSAGE_ACCESSOR(path)
MESSAGE_ACCESSOR(interface)
MESSAGE_ACCESSOR(member)
MESSAGE_ACCESSOR_NOHAS(error_name)
MESSAGE_ACCESSOR(destination)
MESSAGE_ACCESSOR(sender)
MESSAGE_GET_ACCESSOR(signature)
MESSAGE_HAS_ACCESSOR(signature)
/* bool no_reply */

value stub_dbus_message_get_serial(value message)
{
	CAMLparam1(message);
	CAMLlocal1(serial);
	int c_serial;

	c_serial = dbus_message_get_serial(DBusMessage_val(message));
	serial = caml_copy_int32(c_serial);
	CAMLreturn(serial);
}

value stub_dbus_message_get_reply_serial(value message)
{
	CAMLparam1(message);
	CAMLlocal1(serial);
	int c_serial;

	c_serial = dbus_message_get_reply_serial(DBusMessage_val(message));
	serial = caml_copy_int32(c_serial);
	CAMLreturn(serial);
}

value stub_dbus_message_set_reply_serial(value message, value serial)
{
	CAMLparam2(message, serial);
	unsigned int c_serial = Int32_val(serial);

	dbus_message_set_reply_serial(DBusMessage_val(message), c_serial);
	CAMLreturn(Val_unit);
}

value stub_dbus_message_get_auto_start(value message)
{
	CAMLparam1(message);
	int ret;

	ret = dbus_message_get_auto_start(DBusMessage_val(message));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_message_set_auto_start(value message, value v)
{
	CAMLparam2(message, v);
	dbus_message_set_auto_start(DBusMessage_val(message), Bool_val(v));
	CAMLreturn(Val_unit);
}

value stub_dbus_message_append(value message, value list)
{
	CAMLparam2(message, list);
	CAMLlocal3(tmp, type, v);
	DBusMessage *c_msg;
	DBusMessageIter iter;

	c_msg = DBusMessage_val(message);
	dbus_message_iter_init_append(c_msg, &iter);
	tmp = list;
	while (tmp != Val_emptylist) {
		int c_type;

		type = Field(tmp, 0);
		c_type = __type_table[Tag_val(type)];
		v = Field(type, 0);
		switch (c_type) {
		case DBUS_TYPE_BYTE: {
			char x;
			x = Int_val(v);
			dbus_message_iter_append_basic(&iter, c_type, &x);
			break;
			}
		case DBUS_TYPE_BOOLEAN: {
			int x;
			x = Bool_val(v);
			dbus_message_iter_append_basic(&iter, c_type, &x);
			break;
			}
		case DBUS_TYPE_UINT16:
		case DBUS_TYPE_INT16: {
			int x;
			x = Int_val(v);
			dbus_message_iter_append_basic(&iter, c_type, &x);
			break;
			}
		case DBUS_TYPE_UINT32:
		case DBUS_TYPE_INT32: {
			int x;
			x = Int32_val(v);
			dbus_message_iter_append_basic(&iter, c_type, &x);
			break;
			}
		case DBUS_TYPE_UINT64:
		case DBUS_TYPE_INT64: {
			unsigned long long x;
			x = Int64_val(v);
			dbus_message_iter_append_basic(&iter, c_type, &x);
			break;
			}
		case DBUS_TYPE_DOUBLE: {
			double d;
			d = Double_val(v);
			dbus_message_iter_append_basic(&iter, c_type, &d);
			break;
			}
		case DBUS_TYPE_STRING: {
			char *s = strdup(String_val(v));
			dbus_message_iter_append_basic(&iter, c_type, &s);
			break;
			}
		default:
			caml_failwith("internal error");
			break;
		}
		tmp = Field(tmp, 1);
	}
	CAMLreturn(Val_unit);
}

value stub_dbus_message_get(value message)
{
	CAMLparam1(message);
	CAMLlocal4(tmp, list, v, r);
	DBusMessage *c_msg;
	DBusMessageIter args;
	int has_next;

	c_msg = DBusMessage_val(message);
	tmp = list = Val_emptylist;
	v = Val_unit;

	has_next = dbus_message_iter_init(c_msg, &args);
	while (has_next) {
		int c_type, type;

		c_type = dbus_message_iter_get_arg_type(&args);
		type = find_index_equal(c_type, __type_table);
		switch (c_type) {
		case DBUS_TYPE_BYTE: {
			char c;
			dbus_message_iter_get_basic(&args, &c);
			v = Val_int(c);
			r = caml_alloc_small(1, type);
			Field(r, 0) = v;
			break;
			}
		case DBUS_TYPE_BOOLEAN: {
			int i;
			dbus_message_iter_get_basic(&args, &i);
			v = Val_bool(i);
			r = caml_alloc_small(1, type);
			Field(r, 0) = v;
			break;
			}
		case DBUS_TYPE_UINT16:
		case DBUS_TYPE_INT16: {
			int i;
			dbus_message_iter_get_basic(&args, &i);
			v = Val_int(i);
			r = caml_alloc_small(1, type);
			Field(r, 0) = v;
			break;
			}
		case DBUS_TYPE_UINT32:
		case DBUS_TYPE_INT32: {
			int i;
			dbus_message_iter_get_basic(&args, &i);
			v = caml_copy_int32(i);
			r = caml_alloc_small(1, type);
			Field(r, 0) = v;
			break;
			}
		case DBUS_TYPE_UINT64:
		case DBUS_TYPE_INT64: {
			unsigned long long ld;
			dbus_message_iter_get_basic(&args, &ld);
			v = caml_copy_int64(ld);
			r = caml_alloc_small(1, type);
			Field(r, 0) = v;
			break;
			}
		case DBUS_TYPE_STRING: {
			char *s;
			dbus_message_iter_get_basic(&args, &s);
			v = caml_copy_string(s);
			r = caml_alloc_small(1, type);
			Field(r, 0) = v;
			break;
			}
		case DBUS_TYPE_DOUBLE: {
			double d;
			dbus_message_iter_get_basic(&args, &d);
			v = caml_copy_double(d);
			r = caml_alloc_small(1, type);
			Field(r, 0) = v;
			break;
			}
		default:
			caml_failwith("unexpected type in message");
			v = Val_unit;
			break;
		}

		tmp = caml_alloc_small(2, Tag_cons);
		Field(tmp, 0) = r;
		Field(tmp, 1) = list;
		list = tmp;

		has_next = dbus_message_iter_next(&args);
	}
	CAMLreturn(list);
}

value stub_dbus_message_is_signal(value message, value interface,
                                  value signal_name)
{
	CAMLparam3(message, interface, signal_name);
	int ret;

	ret = dbus_message_is_signal(DBusMessage_val(message),
	                             String_val(interface),
	                             String_val(signal_name));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_message_is_method_call(value message, value interface,
                                       value method_name)
{
	CAMLparam3(message, interface, method_name);
	int ret;

	ret = dbus_message_is_method_call(DBusMessage_val(message),
	                                  String_val(interface),
	                                  String_val(method_name));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_message_is_error(value message, value error_name)
{
	CAMLparam2(message, error_name);
	int ret;

	ret = dbus_message_is_error(DBusMessage_val(message),
	                            String_val(error_name));
	CAMLreturn(Val_bool(ret));
}

/**************** PENDING CALL ******************/
value stub_dbus_pending_call_block(value pending)
{
	CAMLparam1(pending);
	dbus_pending_call_block(DBusPendingCall_val(pending));
	CAMLreturn(Val_unit);
}

value stub_dbus_pending_call_cancel(value pending)
{
	CAMLparam1(pending);
	dbus_pending_call_cancel(DBusPendingCall_val(pending));
	CAMLreturn(Val_unit);
}

value stub_dbus_pending_call_get_completed(value pending)
{
	CAMLparam1(pending);
	int ret;

	ret = dbus_pending_call_get_completed(DBusPendingCall_val(pending));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_pending_call_steal_reply(value pending)
{
	CAMLparam1(pending);
	CAMLlocal1(message);
	DBusMessage *c_message;

	c_message = dbus_pending_call_steal_reply(DBusPendingCall_val(pending));
	if (!c_message)
		caml_failwith("dbus_pending_call_steal_reply");
	voidstar_alloc(message, c_message, finalize_dbus_message);

	CAMLreturn(message);
}
