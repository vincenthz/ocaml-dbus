/*
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

#define Val_none 		(Val_int(0))
#define caml_alloc_variant(val, tag)	\
	do { val = Val_int(tag); } while (0)
#define caml_alloc_variant_param(val, tag, p) \
	do { val = caml_alloc_small(1, tag); Field(val, 0) = (p); } while (0)
#define caml_alloc_some(val, param) \
	caml_alloc_variant_param(val, 0, param)
#define caml_append_list(list, tmp, e)			\
	do {						\
		tmp = caml_alloc_small(2, Tag_cons);	\
		Field(tmp, 0) = (e);			\
		Field(tmp, 1) = list;			\
		list = tmp;				\
	} while (0)
#define iterate_caml_list(list, tmp)			\
	(tmp = (list); tmp != Val_emptylist; tmp = Field(tmp, 1))

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

static int __type_sig_table[] = {
	DBUS_TYPE_BYTE, DBUS_TYPE_BOOLEAN,
	DBUS_TYPE_INT16, DBUS_TYPE_UINT16,
	DBUS_TYPE_INT32, DBUS_TYPE_UINT32,
	DBUS_TYPE_INT64, DBUS_TYPE_UINT64,
	DBUS_TYPE_DOUBLE, DBUS_TYPE_STRING,
	DBUS_TYPE_OBJECT_PATH, DBUS_TYPE_VARIANT,
	DBUS_TYPE_ARRAY, DBUS_TYPE_STRUCT, DBUS_TYPE_DICT_ENTRY,
	-1
};

static int __type_array_table[] = { /* +1 */
	DBUS_TYPE_BYTE, DBUS_TYPE_BOOLEAN,
	DBUS_TYPE_INT16, DBUS_TYPE_UINT16,
	DBUS_TYPE_INT32, DBUS_TYPE_UINT32,
	DBUS_TYPE_INT64, DBUS_TYPE_UINT64,
	DBUS_TYPE_DOUBLE, DBUS_TYPE_STRING,
	DBUS_TYPE_OBJECT_PATH,
	DBUS_TYPE_STRUCT,
	DBUS_TYPE_VARIANT,
	DBUS_TYPE_DICT_ENTRY,
	-1
};

static int __type_table[] = { /* +1 */
	DBUS_TYPE_BYTE, DBUS_TYPE_BOOLEAN,
	DBUS_TYPE_INT16, DBUS_TYPE_UINT16,
	DBUS_TYPE_INT32, DBUS_TYPE_UINT32,
	DBUS_TYPE_INT64, DBUS_TYPE_UINT64,
	DBUS_TYPE_DOUBLE, DBUS_TYPE_STRING,
	DBUS_TYPE_OBJECT_PATH, DBUS_TYPE_ARRAY,
	DBUS_TYPE_STRUCT, DBUS_TYPE_VARIANT,
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
#define DBusWatch_val(v)        ((DBusWatch *) Field(v, 1))

#define voidstar_alloc(o_con, c_con, final_fct)				\
	o_con = caml_alloc_final(SIZEOF_FINALPTR, final_fct,		\
	                         SIZEOF_FINALPTR, 10 * SIZEOF_FINALPTR);\
	Store_field(o_con, 1, (value) c_con);

void finalize_dbus_connection(value v)
{
	dbus_connection_close(DBusConnection_val(v));
	dbus_connection_unref(DBusConnection_val(v));
}

void finalize_dbus_connection_unref(value v)
{
	dbus_connection_unref(DBusConnection_val(v));
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

void finalize_dbus_watch(value v)
{
	/* empty */
}

static void raise_dbus_error(DBusError *error)
{
	static value *dbus_err = NULL;
	value args[2];

	if (!dbus_err)
		dbus_err = caml_named_value("dbus.error");
	args[0] = caml_copy_string(error->name ? error->name : "");
	args[1] = caml_copy_string(error->message ? error->message : "");
	caml_raise_with_args(*dbus_err, 2, args);
}

/******************** BUS **********************/
value stub_dbus_bus_get(value type)
{
	CAMLparam1(type);
	CAMLlocal1(con);
	DBusConnection *c_con;
	DBusError error;

	dbus_error_init(&error);
	c_con = dbus_bus_get(__bustype_table[Int_val(type)], &error);
	if (!c_con)
		raise_dbus_error(&error);

	voidstar_alloc(con, c_con, finalize_dbus_connection_unref);
	CAMLreturn(con);
}

value stub_dbus_bus_get_private(value type)
{
	CAMLparam1(type);
	CAMLlocal1(con);
	DBusConnection *c_con;
	DBusError error;

	dbus_error_init(&error);
	c_con = dbus_bus_get_private(__bustype_table[Int_val(type)], &error);
	if (!c_con)
		raise_dbus_error(&error);

	voidstar_alloc(con, c_con, finalize_dbus_connection);
	CAMLreturn(con);
}

value stub_dbus_bus_register(value bus)
{
	CAMLparam1(bus);
	DBusError error;
	int ret;

	dbus_error_init(&error);
	ret = dbus_bus_register(DBusConnection_val(bus), &error);
	if (ret != TRUE)
		raise_dbus_error(&error);
	CAMLreturn(Val_unit);
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
	ret = caml_copy_string(s);
	CAMLreturn(ret);
}

value stub_dbus_bus_request_name(value bus, value name, value flags)
{
	CAMLparam3(bus, name, flags);
	DBusError error;
	int ret;

	dbus_error_init(&error);
	ret = dbus_bus_request_name(DBusConnection_val(bus), String_val(name),
	                            Int_val(flags), &error);
	if (ret == -1)
		raise_dbus_error(&error);
	CAMLreturn(Val_unit);
}

value stub_dbus_bus_release_name(value bus, value name)
{
	CAMLparam2(bus, name);
	DBusError error;
	int ret;

	dbus_error_init(&error);
	ret = dbus_bus_release_name(DBusConnection_val(bus), String_val(name), &error);
	if (ret == -1)
		raise_dbus_error(&error);
	CAMLreturn(Val_unit);
}

value stub_dbus_bus_has_owner(value bus, value name)
{
	CAMLparam2(bus, name);
	DBusError error;
	int ret;

	dbus_error_init(&error);
	ret = dbus_bus_name_has_owner(DBusConnection_val(bus), String_val(name), &error);
	if (ret != TRUE && dbus_error_is_set(&error))
		raise_dbus_error(&error);
	CAMLreturn(Val_bool(ret == TRUE));
}

value stub_dbus_bus_add_match(value bus, value s, value blocking)
{
	CAMLparam3(bus, s, blocking);
	DBusError error;

	dbus_error_init(&error);
	dbus_bus_add_match(DBusConnection_val(bus), String_val(s),
	                   (Bool_val(blocking) ? &error : NULL));
	if (Bool_val(blocking) && dbus_error_is_set(&error))
		raise_dbus_error(&error);

	CAMLreturn(Val_unit);
}

value stub_dbus_bus_remove_match(value bus, value s, value blocking)
{
	CAMLparam3(bus, s, blocking);
	DBusError error;

	dbus_error_init(&error);
	dbus_bus_remove_match(DBusConnection_val(bus), String_val(s),
	                      (Bool_val(blocking) ? &error : NULL));
	if (Bool_val(blocking) && dbus_error_is_set(&error))
		raise_dbus_error(&error);

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
                                                     value timeout)
{
	CAMLparam3(bus, message, timeout);
	CAMLlocal1(rmsg);
	DBusMessage *c_rmsg;
	DBusError error;

	dbus_error_init(&error);
	c_rmsg = dbus_connection_send_with_reply_and_block(
	                                               DBusConnection_val(bus),
	                                               DBusMessage_val(message),
	                                               Int_val (timeout),
	                                               &error);
	if (!c_rmsg)
		raise_dbus_error(&error);

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

	dbus_connection_ref(connection);
	voidstar_alloc(conn, connection, finalize_dbus_connection_unref);
	dbus_message_ref(message);
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
		caml_alloc_some(msg_opt, msg);
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

static dbus_bool_t watch_add_cb(DBusWatch *c_watch, void *data)
{
	CAMLparam0();
	CAMLlocal1(watch);
	value *fns = data;
	value add_cb = Field(*fns, 0);
	int ret;

	voidstar_alloc(watch, c_watch, finalize_dbus_watch);
	ret = Bool_val(caml_callback(add_cb, watch));
	CAMLreturn(ret);
}

static void watch_rm_cb(DBusWatch *c_watch, void *data)
{
	CAMLparam0();
	CAMLlocal1(watch);
	value *fns = data;
	value rm_cb = Field(*fns, 1);

	voidstar_alloc(watch, c_watch, finalize_dbus_watch);
	caml_callback(rm_cb, watch);
	CAMLreturn0;
}

static void watch_toggle_cb(DBusWatch *c_watch, void *data)
{
	CAMLparam0();
	CAMLlocal1(watch);
	value *fns = data;
	value toggle_cb = Field(*fns, 2);

	if (toggle_cb != Val_none) {
		voidstar_alloc(watch, c_watch, finalize_dbus_watch);
		caml_callback(Field(toggle_cb, 0), watch);
	}
	CAMLreturn0;
}

static void watch_free_cb(void *data)
{
	value *v = data;
	caml_remove_global_root(v);
	free(v);
}

value stub_dbus_connection_set_watch_functions(value bus, value fns)
{
	CAMLparam2(bus, fns);
	value *callbackfns;

	callbackfns = malloc(sizeof(value));
	if (!callbackfns)
		caml_raise_out_of_memory();

	*callbackfns = fns;
	caml_register_global_root(callbackfns);

	dbus_connection_set_watch_functions(DBusConnection_val(bus), watch_add_cb,
	                                    watch_rm_cb, watch_toggle_cb, (void *) fns,
	                                    watch_free_cb);
	CAMLreturn(Val_unit);
}

value stub_dbus_connection_set_max_message_size(value bus, value size)
{
	CAMLparam2(bus, size);
	dbus_connection_set_max_message_size(DBusConnection_val(bus), Int_val(size));
	CAMLreturn(Val_unit);
}

value stub_dbus_connection_get_max_message_size(value bus)
{
	CAMLparam1(bus);
	long ret;
	ret = dbus_connection_get_max_message_size(DBusConnection_val(bus));
	CAMLreturn(Val_int(ret));
}

value stub_dbus_connection_set_max_received_size(value bus, value size)
{
	CAMLparam2(bus, size);
	dbus_connection_set_max_received_size(DBusConnection_val(bus), Int_val(size));
	CAMLreturn(Val_unit);
}

value stub_dbus_connection_get_max_received_size(value bus)
{
	CAMLparam1(bus);
	long ret;
	ret = dbus_connection_get_max_received_size(DBusConnection_val(bus));
	CAMLreturn(Val_int(ret));
}

value stub_dbus_connection_get_outgoing_size(value bus)
{
	CAMLparam1(bus);
	long ret;
	ret = dbus_connection_get_outgoing_size(DBusConnection_val(bus));
	CAMLreturn(Val_int(ret));
}

value stub_dbus_connection_set_allow_anonymous(value bus, value val)
{
	CAMLparam2(bus, val);
	dbus_connection_set_allow_anonymous(DBusConnection_val(bus), Bool_val(val));
	CAMLreturn(Val_unit);
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

value stub_dbus_message_new_method_call(value destination, value path,
                                        value interface, value method)
{
	CAMLparam4(destination, path, interface, method);
	CAMLlocal1(msg);
	DBusMessage *c_msg;

	c_msg = dbus_message_new_method_call(String_val(destination),
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
	caml_alloc_some(v, vfield);				\
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

#define IS_BASIC(ty) \
	((ty) == DBUS_TYPE_BYTE || \
	 (ty) == DBUS_TYPE_BOOLEAN || \
	 (ty) == DBUS_TYPE_UINT16 || (ty) == DBUS_TYPE_INT16 || \
	 (ty) == DBUS_TYPE_UINT32 || (ty) == DBUS_TYPE_INT32 || \
	 (ty) == DBUS_TYPE_UINT64 || (ty) == DBUS_TYPE_INT64 || \
	 (ty) == DBUS_TYPE_DOUBLE || \
	 (ty) == DBUS_TYPE_OBJECT_PATH || \
	 (ty) == DBUS_TYPE_STRING)

static void message_append_basic(DBusMessageIter *iter, int c_type, value v)
{
	switch (c_type) {
	case DBUS_TYPE_BYTE: {
		char x;
		x = Int_val(v);
		dbus_message_iter_append_basic(iter, c_type, &x);
		break;
		}
	case DBUS_TYPE_BOOLEAN: {
		int x;
		x = Bool_val(v);
		dbus_message_iter_append_basic(iter, c_type, &x);
		break;
		}
	case DBUS_TYPE_UINT16:
	case DBUS_TYPE_INT16: {
		int x;
		x = Int_val(v);
		dbus_message_iter_append_basic(iter, c_type, &x);
		break;
		}
	case DBUS_TYPE_UINT32:
	case DBUS_TYPE_INT32: {
		int x;
		x = Int32_val(v);
		dbus_message_iter_append_basic(iter, c_type, &x);
		break;
		}
	case DBUS_TYPE_UINT64:
	case DBUS_TYPE_INT64: {
		unsigned long long x;
		x = Int64_val(v);
		dbus_message_iter_append_basic(iter, c_type, &x);
		break;
		}
	case DBUS_TYPE_DOUBLE: {
		double d;
		d = Double_val(v);
		dbus_message_iter_append_basic(iter, c_type, &d);
		break;
		}
	case DBUS_TYPE_OBJECT_PATH:
	case DBUS_TYPE_STRING: {
		char *s = strdup(String_val(v));
		dbus_message_iter_append_basic(iter, c_type, &s);
		break;
		}
	default:
		break;
	}
}

static value message_append_array(DBusMessageIter *iter, value array)
{
	CAMLparam1(array);
	CAMLlocal1(tmp);
	DBusMessageIter sub;
	int array_c_type;
	char signature[256];
	memset(signature, 0, sizeof(signature));

	array_c_type = __type_array_table[Tag_val(array)];
	if (IS_BASIC(array_c_type)) {
		signature[0] = (char) array_c_type;

		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature, &sub);
		for iterate_caml_list(Field(array, 0), tmp) {
			message_append_basic(&sub, array_c_type, Field(tmp, 0));
		}
		dbus_message_iter_close_container(iter, &sub);
	} else if (array_c_type == DBUS_TYPE_STRUCT) {
		int so = 0;
		caml_failwith("array of struct not supported yet");
		signature[so++] = '(';
		/* fixme */
		signature[so++] = ')';

		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature, &sub);
		dbus_message_iter_close_container(iter, &sub);
	} else if (array_c_type == DBUS_TYPE_VARIANT) {
		caml_failwith("array of varient not supported yet");
		signature[0] = 'v';
	} else if (array_c_type == DBUS_TYPE_DICT_ENTRY) {
		/* ocaml representation: Dicts of (ty_sig * ty_sig) * ((ty * ty) list) */
		int ksig, vsig;
		value sigtuple = Field(array, 0);

		if (Is_block(Field(sigtuple, 0)))
			caml_failwith("dictionnary entry key cannot be a container type");
		if (Is_block(Field(sigtuple, 1)))
			caml_failwith("dictionnary entry value cannot be a container type");
		ksig = __type_sig_table[Int_val(Field(sigtuple, 0))];
		vsig = __type_sig_table[Int_val(Field(sigtuple, 1))];

		signature[0] = '{';
		signature[1] = ksig;
		signature[2] = vsig;
		signature[3] = '}';

		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature, &sub);
		for iterate_caml_list(Field(array, 1), tmp) {
			value tuple = Field(tmp, 0);

			message_append_basic(&sub, ksig, Field(tuple, 0));
			/* FIXME if VARIANT need to go inside the stuff */
			if (vsig == DBUS_TYPE_VARIANT)
				;
			message_append_basic(&sub, vsig, Field(tuple, 1));
		}
		dbus_message_iter_close_container(iter, &sub);
	} else
		caml_failwith("internal error");
	CAMLreturn(Val_unit);
}

static value message_append_rec(DBusMessageIter *iter, value list)
{
	CAMLparam1(list);
	CAMLlocal3(tmp, type, v);

	for iterate_caml_list(list, tmp) {
		int c_type;

		type = Field(tmp, 0);
		c_type = __type_table[Tag_val(type)];
		v = Field(type, 0);
		if (IS_BASIC(c_type)) {
			message_append_basic(iter, c_type, v);
		} else if (c_type == DBUS_TYPE_ARRAY) {
			message_append_array(iter, v);
		} else {
			caml_failwith("internal error");
		}
	}
	CAMLreturn(Val_unit);
}

value stub_dbus_message_append(value message, value list)
{
	CAMLparam2(message, list);
	DBusMessage *c_msg;
	DBusMessageIter iter;

	c_msg = DBusMessage_val(message);
	dbus_message_iter_init_append(c_msg, &iter);
	message_append_rec(&iter, list);

	CAMLreturn(Val_unit);
}

static value dbus_ty_list_from_c(DBusMessageIter *iter, int initial_has_next, int alloc_variant);

static value message_basic_type_to_caml(DBusMessageIter *iter, int c_type)
{
	CAMLparam0();
	CAMLlocal1(v);
	switch (c_type) {
	case DBUS_TYPE_BYTE: {
		char c;
		dbus_message_iter_get_basic(iter, &c);
		v = Val_int(c);
		break;
		}
	case DBUS_TYPE_BOOLEAN: {
		int i;
		dbus_message_iter_get_basic(iter, &i);
		v = Val_bool(i);
		break;
		}
	case DBUS_TYPE_UINT16:
	case DBUS_TYPE_INT16: {
		int i;
		dbus_message_iter_get_basic(iter, &i);
		v = Val_int(i);
		break;
		}
	case DBUS_TYPE_UINT32:
	case DBUS_TYPE_INT32: {
		int i;
		dbus_message_iter_get_basic(iter, &i);
		v = caml_copy_int32(i);
		break;
		}
	case DBUS_TYPE_UINT64:
	case DBUS_TYPE_INT64: {
		unsigned long long ld;
		dbus_message_iter_get_basic(iter, &ld);
		v = caml_copy_int64(ld);
		break;
		}
	case DBUS_TYPE_OBJECT_PATH:
	case DBUS_TYPE_STRING: {
		char *s;
		dbus_message_iter_get_basic(iter, &s);
		v = caml_copy_string(s);
		break;
		}
	case DBUS_TYPE_DOUBLE: {
		double d;
		dbus_message_iter_get_basic(iter, &d);
		v = caml_copy_double(d);
		break;
		}
	default:
		v = Val_int(0);
		break;
	}
	CAMLreturn(v);
}

static value dbus_array_to_caml(DBusMessageIter *iter, int array_c_type, int initial_has_next)
{
	CAMLparam0();
	CAMLlocal2(r, v);
	int type;

	type = find_index_equal(array_c_type, __type_array_table);
	/* check if we know the type */
	if (type == -1)
		caml_alloc_variant(r, 0);
	else {
		if (IS_BASIC(array_c_type)) {
			/* basic are all in the form : BASIC of list */
			v = dbus_ty_list_from_c(iter, 1, 0);
			caml_alloc_variant_param(r, type, v);
		} else if (array_c_type == DBUS_TYPE_DICT_ENTRY) {
			value sig, listv;
			DBusMessageIter sub;
			int offset = 0;

			listv = Val_emptylist;
			sig = caml_alloc_tuple(2);
			dbus_message_iter_recurse(iter, &sub);
			do {
				int c_type, type;

				c_type = dbus_message_iter_get_arg_type(&sub);
				if (IS_BASIC(c_type) || c_type == DBUS_TYPE_VARIANT)
					type = find_index_equal(c_type, __type_sig_table);
				else
					caml_failwith("dict entry are container ?");
				if (offset > 1)
					continue;
				Field(sig, offset) = Val_int(type);
				offset++;
			} while (dbus_message_iter_next(&sub));

			v = caml_alloc_tuple(2);
			Field(v, 0) = sig;
			Field(v, 1) = listv;

			caml_alloc_variant_param(r, type, v);
			caml_failwith("array of dict entry not supported");
		} else if (array_c_type == DBUS_TYPE_VARIANT) {
			caml_failwith("array of variant not supported");
		} else
			caml_failwith("array of container not supported");
	}

	CAMLreturn(r);
}

static value dbus_struct_to_caml(DBusMessageIter *iter, int initial_has_next)
{
	CAMLparam0();
	value v;

	v = dbus_ty_list_from_c(iter, 1, 1);
	CAMLreturn(v);
}

/** dbus_ty_one_from_c returns one value beeing the raw representation of
 *  the type. meaning it's not tagged for use as an ocaml variant type.
 */
static value dbus_ty_one_from_c(DBusMessageIter *iter, int *subtype)
{
	CAMLparam0();
	CAMLlocal1(v);
	int c_type, type;

	c_type = dbus_message_iter_get_arg_type(iter);
	type = find_index_equal(c_type, __type_table);

	if (IS_BASIC(c_type)) {
		v = message_basic_type_to_caml(iter, c_type);
	} else if (c_type == DBUS_TYPE_ARRAY) {
		DBusMessageIter sub;
		dbus_message_iter_recurse(iter, &sub);

		v = dbus_array_to_caml(&sub, dbus_message_iter_get_element_type(iter), 1);
	} else if (c_type == DBUS_TYPE_STRUCT) {
		DBusMessageIter sub;
		dbus_message_iter_recurse(iter, &sub);

		v = dbus_struct_to_caml(&sub, 1);
	} else if (c_type == DBUS_TYPE_VARIANT) {
		DBusMessageIter sub;
		int subtype;
		value r;

		dbus_message_iter_recurse(iter, &sub);
		v = dbus_ty_one_from_c(&sub, &subtype);
		caml_alloc_variant_param(r, subtype, v);
		v = r;
	} else {
		caml_alloc_variant(v, 0);
	}

	if (subtype)
		*subtype = type;
	CAMLreturn(v);
}

/** dbus_ty_list_from_c returns a caml list of value.
 * if alloc_variant is true, then we allocated the ocaml variant-type tag. List of Dbus.ty
 * otherwise, we allocated a raw list of values. List of string, List of int, etc
 */
static value dbus_ty_list_from_c(DBusMessageIter *iter, int initial_has_next, int alloc_variant)
{
	CAMLparam0();
	CAMLlocal4(tmp, list, v, r);
	int has_next;

	/* initialize local caml values */
	tmp = list = Val_emptylist;
	r = Val_unit;
	v = Val_unit;

	has_next = initial_has_next;
	while (has_next) {
		int subtype;

		v = dbus_ty_one_from_c(iter, &subtype);
		if (alloc_variant) {
			caml_alloc_variant_param(r, subtype, v);
		}
		caml_append_list(list, tmp, (alloc_variant ? r : v));

		has_next = dbus_message_iter_next(iter);
	}
	CAMLreturn(list);
}

value stub_dbus_message_get(value message)
{
	CAMLparam1(message);
	CAMLlocal1(v);
	DBusMessage *c_msg;
	DBusMessageIter args;
	int has_next;

	c_msg = DBusMessage_val(message);
	has_next = dbus_message_iter_init(c_msg, &args);
	v = dbus_ty_list_from_c(&args, has_next, 1);

	CAMLreturn(v);
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

value stub_dbus_watch_get_unix_fd(value watch)
{
	CAMLparam1(watch);
	int c_fd;

	c_fd = dbus_watch_get_unix_fd(DBusWatch_val(watch));
	CAMLreturn(Val_int(c_fd));
}

value stub_dbus_watch_get_enabled(value watch)
{
	CAMLparam1(watch);
	int ret;

	ret = dbus_watch_get_enabled(DBusWatch_val(watch));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_watch_get_flags(value watch)
{
	CAMLparam1(watch);
	CAMLlocal2(flags, tmp);
	unsigned int c_flags;

	flags = Val_emptylist;
	c_flags = dbus_watch_get_flags(DBusWatch_val(watch));
	if (c_flags & DBUS_WATCH_READABLE) {
		caml_append_list(flags, tmp, Val_int(0));
	}
	if (c_flags & DBUS_WATCH_WRITABLE) {
		caml_append_list(flags, tmp, Val_int(1));
	}

	CAMLreturn(flags);
}

value stub_dbus_watch_handle(value watch, value flags)
{
	CAMLparam2(watch, flags);
	unsigned int c_flags;

	for (c_flags = 0; flags != Val_emptylist; flags = Field(flags, 1)) {
		switch (Field(flags, 0)) {
		case 0: c_flags |= DBUS_WATCH_READABLE; break;
		case 1: c_flags |= DBUS_WATCH_WRITABLE; break;
		default: /* ouphfm */ break;
		}
	}
	dbus_watch_handle(DBusWatch_val(watch), c_flags);

	CAMLreturn(Val_unit);
}
