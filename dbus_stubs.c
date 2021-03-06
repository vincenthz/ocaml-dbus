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

#include <stdint.h>
#include <string.h>
#include <dbus/dbus.h>

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>

#if defined(DEBUG_APPEND_ENABLE) || defined(DEBUG_GET_ENABLE) || defined(DEBUG_SIG_ENABLE)
#include <stdio.h>
#include <stdlib.h>
#endif

/* debug */
#ifdef DEBUG_APPEND_ENABLE
#define DEBUG_APPEND(fmt, ...)	fprintf(stderr, "debug: append: " fmt, ##__VA_ARGS__)
#else
#define DEBUG_APPEND(fmt, ...)	((void) 0)
#endif

#ifdef DEBUG_GET_ENABLE
#define DEBUG_GET(fmt, ...)	fprintf(stderr, "debug: get: " fmt, ##__VA_ARGS__)
#else
#define DEBUG_GET(fmt, ...)	((void) 0)
#endif

#ifdef DEBUG_SIG_ENABLE
#define DEBUG_SIG(fmt, ...)	fprintf(stderr, "debug: sig: " fmt, ##__VA_ARGS__)
#else
#define DEBUG_SIG(fmt, ...)	((void) 0)
#endif

#define Val_none 		(Val_int(0))
#define caml_alloc_variant(val, tag)	\
	do { val = tag; } while (0)
#define caml_alloc_variant_param(val, tag, p) \
	do { val = caml_alloc_small(1, tag); Field(val, 0) = (p); } while (0)
#define caml_alloc_variant_param2(val, tag, p1, p2) \
	do { val = caml_alloc_small(2, tag); Field(val, 0) = (p1); Field(val, 1) = (p2); } while (0)
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

static value caml_list_rev(value list)
{
	CAMLparam1(list);
	CAMLlocal3(tmp, tmprev, listrev);
	listrev = tmp = Val_emptylist;

	for iterate_caml_list(list, tmp) {
		caml_append_list(listrev, tmprev, Field(tmp, 0));
	}
	CAMLreturn(listrev);
}

#define SIZEOF_FINALPTR		(2 * sizeof (void *))

static int __messagetype_table[] = {
	DBUS_MESSAGE_TYPE_INVALID,
	DBUS_MESSAGE_TYPE_METHOD_CALL,
	DBUS_MESSAGE_TYPE_METHOD_RETURN,
	DBUS_MESSAGE_TYPE_ERROR,
	DBUS_MESSAGE_TYPE_SIGNAL,
	-1
};

static int __dispatch_status_table[] = {
	DBUS_DISPATCH_DATA_REMAINS,
	DBUS_DISPATCH_COMPLETE,
	DBUS_DISPATCH_NEED_MEMORY,
	-1
};

static int __bustype_table[] = {
	DBUS_BUS_SESSION, DBUS_BUS_SYSTEM, DBUS_BUS_STARTER, -1
};

static char *__error_table[] = {
	DBUS_ERROR_FAILED,
	DBUS_ERROR_NO_MEMORY,
	DBUS_ERROR_SERVICE_UNKNOWN,
	DBUS_ERROR_NAME_HAS_NO_OWNER,
	DBUS_ERROR_NO_REPLY,
	DBUS_ERROR_IO_ERROR,
	DBUS_ERROR_BAD_ADDRESS,
	DBUS_ERROR_NOT_SUPPORTED,
	DBUS_ERROR_LIMITS_EXCEEDED,
	DBUS_ERROR_ACCESS_DENIED,
	DBUS_ERROR_AUTH_FAILED,
	DBUS_ERROR_NO_SERVER,
	DBUS_ERROR_TIMEOUT,
	DBUS_ERROR_NO_NETWORK,
	DBUS_ERROR_ADDRESS_IN_USE,
	DBUS_ERROR_DISCONNECTED,
	DBUS_ERROR_INVALID_ARGS,
	DBUS_ERROR_FILE_NOT_FOUND,
	DBUS_ERROR_FILE_EXISTS,
	DBUS_ERROR_UNKNOWN_METHOD,
	DBUS_ERROR_TIMED_OUT,
	DBUS_ERROR_MATCH_RULE_NOT_FOUND,
	DBUS_ERROR_MATCH_RULE_INVALID,
	DBUS_ERROR_SPAWN_EXEC_FAILED,
	DBUS_ERROR_SPAWN_FORK_FAILED,
	DBUS_ERROR_SPAWN_CHILD_EXITED,
	DBUS_ERROR_SPAWN_CHILD_SIGNALED,
	DBUS_ERROR_SPAWN_FAILED,
	DBUS_ERROR_SPAWN_SETUP_FAILED,
	DBUS_ERROR_SPAWN_CONFIG_INVALID,
	DBUS_ERROR_SPAWN_SERVICE_INVALID,
	DBUS_ERROR_SPAWN_SERVICE_NOT_FOUND,
	DBUS_ERROR_SPAWN_PERMISSIONS_INVALID,
	DBUS_ERROR_SPAWN_FILE_INVALID,
	DBUS_ERROR_SPAWN_NO_MEMORY,
	DBUS_ERROR_UNIX_PROCESS_ID_UNKNOWN,
	DBUS_ERROR_INVALID_SIGNATURE,
	DBUS_ERROR_INVALID_FILE_CONTENT,
	DBUS_ERROR_SELINUX_SECURITY_CONTEXT_UNKNOWN,
	DBUS_ERROR_ADT_AUDIT_DATA_UNKNOWN,
	DBUS_ERROR_OBJECT_PATH_IN_USE,
	NULL,
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
	DBUS_TYPE_ARRAY,
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

static int find_index_string(const char *searched, char **table)
{
	int i;

	for (i = 0; table[i] != NULL; i++)
		if (strcmp(table[i], searched) == 0)
			return i;
	return -1;
}

#define DBusConnection_val(v)   (*((DBusConnection **) Data_custom_val(v)))
#define DBusMessage_val(v)      (*((DBusMessage **) Data_custom_val(v)))
#define DBusError_val(v)        (*((DBusError **) Data_custom_val(v)))
#define DBusPendingCall_val(v)  (*((DBusPendingCall **) Data_custom_val(v)))
#define DBusWatch_val(v)        (*((DBusWatch **) Data_custom_val(v)))
#define DBusTimeout_val(v)      (*((DBusTimeout **) Data_custom_val(v)))

#define voidstar_alloc(o_con, c_con, final_fct)					\
	do {									\
		o_con = caml_alloc_final(SIZEOF_FINALPTR, final_fct,		\
					 SIZEOF_FINALPTR, 10 * SIZEOF_FINALPTR);\
		*((unsigned long *) Data_custom_val(o_con)) = (unsigned long) c_con; \
	} while (0)

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

void finalize_dbus_timeout(value v)
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

static void raise_dbus_internal_error(char *s)
{
	static value *dbus_err = NULL;

	if (!dbus_err)
		dbus_err = caml_named_value("dbus.internal_error");
	caml_raise_with_string(*dbus_err, s);
}

static void raise_dbus_type_not_supported(char *s)
{
	static value *dbus_err = NULL;

	if (!dbus_err)
		dbus_err = caml_named_value("dbus.type_not_supported");
	caml_raise_with_string(*dbus_err, s);
}

/****************** signature helpers ***********/
struct stub_dbus_sig {
	int offset;
	int error;
	char data[256];
};

static void signature_init(struct stub_dbus_sig *sig)
{
	sig->offset = 0;
	sig->error = 0;
	memset(sig->data, 0, 256);
}

static void signature_append(struct stub_dbus_sig *sig, char c)
{
	if (sig->offset == 256) {
		sig->error++;
		return;
	}
	sig->data[sig->offset++] = c;
}

static int signature_has_error(struct stub_dbus_sig *sig)
{
	return sig->error;
}

static char * signature_data(struct stub_dbus_sig *sig)
{
	return sig->data;
}

#define CHECK_SIG_ERROR(sig) \
		if (signature_has_error(sig)) \
			raise_dbus_type_not_supported("signature is too big");

/****************** RANDOM **********************/
value stub_dbus_string_of_error_name(value errname)
{
	CAMLparam1(errname);
	CAMLlocal1(ret);

	ret = caml_copy_string(__error_table[Int_val(errname)]);
	CAMLreturn(ret);
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
	CAMLreturn(Val_int(ret));
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
	CAMLreturn(Val_int(ret));
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

	caml_enter_blocking_section();
	ret = dbus_connection_send_with_reply(DBusConnection_val(bus),
	                                      DBusMessage_val(message),
	                                      &c_pending, Int_val(timeout));
	caml_leave_blocking_section();
	if (!ret) {
		free(c_pending);
		raise_dbus_internal_error("dbus_connection_send_with_reply");
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
	caml_enter_blocking_section();
	c_rmsg = dbus_connection_send_with_reply_and_block(
	                                               DBusConnection_val(bus),
	                                               DBusMessage_val(message),
	                                               Int_val (timeout),
	                                               &error);
	caml_leave_blocking_section();
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
	caml_enter_blocking_section();
	dbus_connection_flush(DBusConnection_val(bus));
	caml_leave_blocking_section();
	CAMLreturn(Val_unit);
}

value stub_dbus_connection_read_write(value bus, value timeout)
{
	CAMLparam2(bus, timeout);
	dbus_bool_t b;
	caml_enter_blocking_section();
	b = dbus_connection_read_write(DBusConnection_val(bus), Int_val(timeout));
	caml_leave_blocking_section();
	CAMLreturn(Val_bool(b));
}

value stub_dbus_connection_read_write_dispatch(value bus, value timeout)
{
	CAMLparam2(bus, timeout);
	dbus_bool_t b;
	caml_enter_blocking_section();
	b = dbus_connection_read_write_dispatch(DBusConnection_val(bus),
	                                        Int_val(timeout));
	caml_leave_blocking_section();
	CAMLreturn(Val_bool(b));
}

value stub_dbus_connection_pop_message(value bus)
{
	CAMLparam1(bus);
	CAMLlocal2(msg_opt, msg);
	DBusMessage *c_msg;

	msg_opt = Val_none;
	msg = Val_unit;

	caml_enter_blocking_section();
	c_msg = dbus_connection_pop_message(DBusConnection_val(bus));
	caml_leave_blocking_section();
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
		raise_dbus_internal_error("dbus_connection_get_fd");
	CAMLreturn(Val_int(fd));
}

value stub_dbus_connection_dispatch(value bus)
{
	CAMLparam1(bus);
	CAMLlocal1(ret);
	DBusDispatchStatus status;

	caml_enter_blocking_section();
	status = dbus_connection_dispatch(DBusConnection_val(bus));
	caml_enter_blocking_section();

	caml_alloc_variant(ret, Val_int(find_index_equal(status, __dispatch_status_table)));
	CAMLreturn(ret);
}

value stub_dbus_connection_get_dispatch_status(value bus)
{
	CAMLparam1(bus);
	CAMLlocal1(ret);
	DBusDispatchStatus status;

	caml_enter_blocking_section();
	status = dbus_connection_get_dispatch_status(DBusConnection_val(bus));
	caml_leave_blocking_section();

	caml_alloc_variant(ret, Val_int(find_index_equal(status, __dispatch_status_table)));
	CAMLreturn(ret);
}

static dbus_bool_t watch_add_cb(DBusWatch *c_watch, void *data)
{
	CAMLparam0();
	CAMLlocal2(watch, add_cb);
	value *fns = data;
	add_cb = Field(*fns, 0);
	int ret;

	voidstar_alloc(watch, c_watch, finalize_dbus_watch);
	ret = Bool_val(caml_callback(add_cb, watch));
	CAMLreturn(ret);
}

static void watch_rm_cb(DBusWatch *c_watch, void *data)
{
	CAMLparam0();
	CAMLlocal2(watch, rm_cb);
	value *fns = data;
	rm_cb = Field(*fns, 1);

	voidstar_alloc(watch, c_watch, finalize_dbus_watch);
	caml_callback(rm_cb, watch);
	CAMLreturn0;
}

static void watch_toggle_cb(DBusWatch *c_watch, void *data)
{
	CAMLparam0();
	CAMLlocal2(watch, toggle_cb);
	value *fns = data;
	toggle_cb = Field(*fns, 2);

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
	int ret;

	callbackfns = malloc(sizeof(value));
	if (!callbackfns)
		caml_raise_out_of_memory();

	*callbackfns = fns;
	caml_register_global_root(callbackfns);

	ret = dbus_connection_set_watch_functions(DBusConnection_val(bus), watch_add_cb,
	                                          watch_rm_cb, watch_toggle_cb, (void *) callbackfns,
	                                          watch_free_cb);
	if (!ret)
		caml_raise_out_of_memory();
	CAMLreturn(Val_unit);
}

static dbus_bool_t timeout_add_cb(DBusTimeout *c_timeout, void *data)
{
	CAMLparam0();
	CAMLlocal2(timeout, add_cb);
	value *fns = data;
	add_cb = Field(*fns, 0);
	int ret;

	voidstar_alloc(timeout, c_timeout, finalize_dbus_timeout);
	ret = Bool_val(caml_callback(add_cb, timeout));
	CAMLreturn(ret);
}

static void timeout_rm_cb(DBusTimeout *c_timeout, void *data)
{
	CAMLparam0();
	CAMLlocal2(timeout, rm_cb);
	value *fns = data;
	rm_cb = Field(*fns, 1);

	voidstar_alloc(timeout, c_timeout, finalize_dbus_timeout);
	caml_callback(rm_cb, timeout);
	CAMLreturn0;
}

static void timeout_toggle_cb(DBusTimeout *c_timeout, void *data)
{
	CAMLparam0();
	CAMLlocal2(timeout, toggle_cb);
	value *fns = data;
	toggle_cb = Field(*fns, 2);

	if (toggle_cb != Val_none) {
		voidstar_alloc(timeout, c_timeout, finalize_dbus_timeout);
		caml_callback(Field(toggle_cb, 0), timeout);
	}
	CAMLreturn0;
}

static void timeout_free_cb(void *data)
{
	value *v = data;
	caml_remove_global_root(v);
	free(v);
}

value stub_dbus_connection_set_timeout_functions(value bus, value fns)
{
	CAMLparam2(bus, fns);
	value *callbackfns;
	int ret;

	callbackfns = malloc(sizeof(value));
	if (!callbackfns)
		caml_raise_out_of_memory();

	*callbackfns = fns;
	caml_register_global_root(callbackfns);

	ret = dbus_connection_set_timeout_functions(DBusConnection_val(bus), timeout_add_cb,
	                                            timeout_rm_cb, timeout_toggle_cb,
	                                            (void *) callbackfns, timeout_free_cb);
	if (!ret)
		caml_raise_out_of_memory();
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
		raise_dbus_internal_error("message_create");
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
		raise_dbus_internal_error("message_new_method_call");
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
		raise_dbus_internal_error("message_new_method_call");
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
		raise_dbus_internal_error("message_new_signal");

	voidstar_alloc(msg, c_msg, finalize_dbus_message);
	CAMLreturn(msg);
}

value stub_dbus_message_new_error(value reply_to, value error_name,
                                  value error_message)
{
	CAMLparam3(reply_to, error_name, error_message);
	CAMLlocal1(msg);
	DBusMessage *c_msg;
	char *errname;

	errname = __error_table[Int_val(error_name)];
	c_msg = dbus_message_new_error(DBusMessage_val(reply_to),
	                               errname,
	                               String_val(error_message));
	if (!c_msg)
		raise_dbus_internal_error("message_new_error");
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
		CAMLreturn(Val_none);				\
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
MESSAGE_ACCESSOR(destination)
MESSAGE_ACCESSOR(sender)
MESSAGE_GET_ACCESSOR(signature)
MESSAGE_HAS_ACCESSOR(signature)
/* bool no_reply */

value stub_dbus_message_get_error_name(value message)
{
	CAMLparam1(message);
	CAMLlocal1(error_name);
	const char *errname;

	errname = dbus_message_get_error_name(DBusMessage_val(message));
	if (!errname)
		error_name = Val_none;
	else {
		int index = find_index_string(errname, __error_table);
		/* we didn't find the exact error,
		   just return a generic one */
		if (index == -1) index = 0;
		caml_alloc_some(error_name, Val_int(index));
	}
	CAMLreturn(error_name);
}

value stub_dbus_message_set_error_name(value message, value error_name)
{
	CAMLparam2(message, error_name);
	char *errname;

	errname = __error_table[Int_val(error_name)];
	dbus_message_set_error_name(DBusMessage_val(message), errname);

	CAMLreturn(Val_unit);
}

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

value stub_dbus_message_get_header(value message)
{
	CAMLparam1(message);
	CAMLlocal2(ret, v);

	ret = caml_alloc_tuple(8);

	v = stub_dbus_message_get_serial(message);
	Store_field(ret, 0, v);

	v = stub_dbus_message_get_type(message);
	Store_field(ret, 1, v);

	v = stub_dbus_message_get_destination(message);
	Store_field(ret, 2, v);

	v = stub_dbus_message_get_path(message);
	Store_field(ret, 3, v);

	v = stub_dbus_message_get_interface(message);
	Store_field(ret, 4, v);

	v = stub_dbus_message_get_member(message);
	Store_field(ret, 5, v);

	v = stub_dbus_message_get_error_name(message);
	Store_field(ret, 6, v);

	v = stub_dbus_message_get_sender(message);
	Store_field(ret, 7, v);

	CAMLreturn(ret);
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
	DEBUG_APPEND("basic: %c (%d)\n", c_type, c_type);
	switch (c_type) {
	case DBUS_TYPE_BYTE: {
		uint8_t x;
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

static void mk_signature_structs(value vstructs, struct stub_dbus_sig *sig);
static void mk_signature_arrays(value arraysig, struct stub_dbus_sig *sig);

static void mk_signature_sig(value sigval, struct stub_dbus_sig *sig)
{
	if (Is_block(sigval)) {
		int c_type;

		c_type = __type_sig_table[Tag_val(sigval) + 12];
		if (c_type == DBUS_TYPE_ARRAY) {
			signature_append(sig, DBUS_TYPE_ARRAY);
			mk_signature_sig(Field(sigval, 0), sig);
		} else if (c_type == DBUS_TYPE_STRUCT) {
			signature_append(sig, '(');
			value list = Field(sigval, 0);
			for iterate_caml_list(list, list) {
				mk_signature_sig(Field(list, 0), sig);
			}
			signature_append(sig, ')');
		} else if (c_type == DBUS_TYPE_DICT_ENTRY) {
			value ksig = Field(sigval, 0);
			value vsig = Field(sigval, 1);
			signature_append(sig, 'a');
			signature_append(sig, '{');
			mk_signature_sig(ksig, sig);
			mk_signature_sig(vsig, sig);
			signature_append(sig, '}');
		}
	} else {
		int vsig = __type_sig_table[Int_val(sigval)];
		signature_append(sig, vsig);
	}
}

static void mk_signature_dict(value ksig, value vsig, struct stub_dbus_sig *sig)
{
	signature_append(sig, '{');
	mk_signature_sig(ksig, sig);
	mk_signature_sig(vsig, sig);
	signature_append(sig, '}');

	DEBUG_SIG("dict: %s (offset=%d)\n", s, offset);
}

static void mk_signature_array(value ty, struct stub_dbus_sig *sig)
{
	int array_c_type;

	signature_append(sig, DBUS_TYPE_ARRAY);
	array_c_type = __type_array_table[Tag_val(ty)];
	if (IS_BASIC(array_c_type) || array_c_type == DBUS_TYPE_VARIANT) {
		signature_append(sig, array_c_type);
	} else if (array_c_type == DBUS_TYPE_DICT_ENTRY) {
		value sigtuple = Field(ty, 0);
		mk_signature_dict(Field(sigtuple, 0), Field(sigtuple, 1), sig);
	} else if (array_c_type == DBUS_TYPE_STRUCT) {
		mk_signature_structs(ty, sig);
	} else if (array_c_type == DBUS_TYPE_ARRAY) {
		mk_signature_arrays(Field(ty, 0), sig);
	} else {
		raise_dbus_type_not_supported("signature of array of unknown types");
	}
	DEBUG_SIG("array: %s (offset=%d)\n", s, offset);
}

static void mk_signature_struct(value list, struct stub_dbus_sig *sig)
{
	value v;

	signature_append(sig, '(');
	for iterate_caml_list(list, list) {
		int c_type;

		v = Field(list, 0);
		c_type = __type_table[Tag_val(v)];
		if (IS_BASIC(c_type)) {
			signature_append(sig, c_type);
		} else {
			signature_append(sig, '#');
		}
	}
	signature_append(sig, ')');
	DEBUG_SIG("struct: %s (offset=%d)\n", s, offset);
}

static void mk_signature_structs(value vstructs, struct stub_dbus_sig *sig)
{
	value list = Field(vstructs, 0); /* Structs signature */

	signature_append(sig, '(');
	for iterate_caml_list(list, list) {
		mk_signature_sig(Field(list, 0), sig);
	}
	signature_append(sig, ')');
	DEBUG_SIG("structs: %s (offset=%d)\n", s, offset);
}

static void mk_signature_arrays(value arraysig, struct stub_dbus_sig *sig)
{
	signature_append(sig, 'a');
	mk_signature_sig(arraysig, sig);
	DEBUG_SIG("arrays: %s (offset=%d)\n", s, offset);
}

static void mk_signature_variant(value ty, struct stub_dbus_sig *sig)
{
	int c_sub_type;

	c_sub_type = __type_table[Tag_val(ty)];
	DEBUG_APPEND("variant: %c (%d)\n", c_sub_type, c_sub_type);

	if (IS_BASIC(c_sub_type)) {
		signature_append(sig, c_sub_type);
	} else if (c_sub_type == DBUS_TYPE_ARRAY) {
		mk_signature_array(Field(ty, 0), sig);
	} else if (c_sub_type == DBUS_TYPE_STRUCT) {
		mk_signature_struct(Field(ty, 0), sig);
	} else {
		/* FIXME once we know howto generate complex signature out of dbus.ty this can be removed */
		raise_dbus_type_not_supported("container type in variant");
	}
	DEBUG_SIG("variant: %s (offset=%d)\n", s, offset);
}

/* forward declaration since we use them recursively in array, struct .. */
static value message_append_one(DBusMessageIter *iter, value v);
static value message_append_list(DBusMessageIter *iter, value list);
static value message_append_variant(DBusMessageIter *iter, value v);
static value message_append_struct(DBusMessageIter *iter, value tylist);

/** message_append array take the array values and append them to the iter */
static value message_append_array(DBusMessageIter *iter, value array)
{
	CAMLparam1(array);
	CAMLlocal1(tmp);
	DBusMessageIter sub;
	int array_c_type;
	struct stub_dbus_sig sig;

	signature_init(&sig);

	array_c_type = __type_array_table[Tag_val(array)];
	DEBUG_APPEND("array: %c (%d)\n", array_c_type, array_c_type);
	if (IS_BASIC(array_c_type)) {
		signature_append(&sig, array_c_type);

		CHECK_SIG_ERROR(&sig);
		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature_data(&sig), &sub);
		for iterate_caml_list(Field(array, 0), tmp) {
			message_append_basic(&sub, array_c_type, Field(tmp, 0));
		}
		dbus_message_iter_close_container(iter, &sub);
	} else if (array_c_type == DBUS_TYPE_STRUCT) {
		/* ocaml representation: Structs of ty_sig list * (ty list list) */
		mk_signature_structs(array, &sig);

		CHECK_SIG_ERROR(&sig);
		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature_data(&sig), &sub);
		for iterate_caml_list(Field(array, 1), tmp) {
			message_append_struct(&sub, Field(tmp, 0));
		}
		dbus_message_iter_close_container(iter, &sub);
	} else if (array_c_type == DBUS_TYPE_VARIANT) {
		signature_append(&sig, 'v');

		CHECK_SIG_ERROR(&sig);
		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature_data(&sig), &sub);
		for iterate_caml_list(Field(array, 0), tmp) {
			message_append_variant(&sub, Field(tmp, 0));
		}
		dbus_message_iter_close_container(iter, &sub);
	} else if (array_c_type == DBUS_TYPE_DICT_ENTRY) {
		/* ocaml representation: Dicts of (ty_sig * ty_sig) * ((ty * ty) list) */
		value sigtuple = Field(array, 0);

		if (Is_block(Field(sigtuple, 0)))
			raise_dbus_type_not_supported("dict entry key cannot be a container type");
		mk_signature_dict(Field(sigtuple, 0), Field(sigtuple, 1), &sig);

		CHECK_SIG_ERROR(&sig);
		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature_data(&sig), &sub);
		for iterate_caml_list(Field(array, 1), tmp) {
			DBusMessageIter subitem;
			value tuple = Field(tmp, 0);

			dbus_message_iter_open_container(&sub, DBUS_TYPE_DICT_ENTRY, NULL, &subitem);
			message_append_one(&subitem, Field(tuple, 0));
			message_append_one(&subitem, Field(tuple, 1));
			dbus_message_iter_close_container(&sub, &subitem);
		}
		dbus_message_iter_close_container(iter, &sub);
	} else if (array_c_type == DBUS_TYPE_ARRAY) {
		/* ocaml representation: Arrays of ty_sig * ty_array list */
		mk_signature_arrays(Field(array, 0), &sig);

		CHECK_SIG_ERROR(&sig);
		dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY, signature_data(&sig), &sub);
		for iterate_caml_list(Field(array, 1), tmp) {
			message_append_array(&sub, Field(tmp, 0));
		}
		dbus_message_iter_close_container(iter, &sub);
	} else
		raise_dbus_internal_error("append array: unknown type");
	CAMLreturn(Val_unit);
}

static value message_append_variant(DBusMessageIter *iter, value v)
{
	CAMLparam1(v);
	DBusMessageIter sub;
	struct stub_dbus_sig sig;

	signature_init(&sig);

	mk_signature_variant(v, &sig);

	CHECK_SIG_ERROR(&sig);
	dbus_message_iter_open_container(iter, DBUS_TYPE_VARIANT, signature_data(&sig), &sub);
	message_append_one(&sub, v);
	dbus_message_iter_close_container(iter, &sub);

	CAMLreturn(Val_unit);
}

static value message_append_struct(DBusMessageIter *iter, value tylist)
{
	CAMLparam1(tylist);

	DBusMessageIter sub;
	dbus_message_iter_open_container(iter, DBUS_TYPE_STRUCT, NULL, &sub);
	message_append_list(&sub, tylist);
	dbus_message_iter_close_container(iter, &sub);

	CAMLreturn(Val_unit);
}

/** message_append_one take ony DBus.ty and append it to the iterator */
static value message_append_one(DBusMessageIter *iter, value v)
{
	CAMLparam1(v);
	int c_type;

	c_type = __type_table[Tag_val(v)];
	DEBUG_APPEND("one: %c (%d)\n", c_type, c_type);
	v = Field(v, 0); /* after this point v represent the contents of the ocaml variant type */
	if (IS_BASIC(c_type)) {
		message_append_basic(iter, c_type, v);
	} else if (c_type == DBUS_TYPE_ARRAY) {
		message_append_array(iter, v);
	} else if (c_type == DBUS_TYPE_STRUCT) {
		message_append_struct(iter, v);
	} else if (c_type == DBUS_TYPE_VARIANT) {
		message_append_variant(iter, v);
	} else {
		/*printf("c_type: %c (%d)\n", c_type, c_type); */
		raise_dbus_internal_error("appending fail: unknown type");
	}
	CAMLreturn(Val_unit);
}

/** message_append_list take a list of DBus.ty and append them to the iterator */
static value message_append_list(DBusMessageIter *iter, value list)
{
	CAMLparam1(list);
	CAMLlocal2(tmp, v);
	DEBUG_APPEND("list\n");

	for iterate_caml_list(list, tmp) {
		v = Field(tmp, 0);
		message_append_one(iter, v);
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
	message_append_list(&iter, list);

	CAMLreturn(Val_unit);
}

static value message_get_one(DBusMessageIter *iter, int *subtype);
static value message_get_list(DBusMessageIter *iter, int initial_has_next, int alloc_variant);
static value message_get_array(DBusMessageIter *iter, int array_c_type, int initial_has_next);

static value message_get_basic(DBusMessageIter *iter, int c_type)
{
	CAMLparam0();
	CAMLlocal1(v);

	DEBUG_GET("basic: %c (%d)\n", c_type, c_type);
	switch (c_type) {
	case DBUS_TYPE_BYTE: {
		uint8_t c;
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

static value message_get_array_struct(DBusMessageIter *iter)
{
	CAMLparam0();
	CAMLlocal3(tmp, list, v);
	int has_next;

	DEBUG_GET("array_struct\n");
	list = tmp = Val_emptylist;
	has_next = dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_INVALID;
	while (has_next) {
		DBusMessageIter sub;

		dbus_message_iter_recurse(iter, &sub);
		v = message_get_list(&sub, 1, 1);
		caml_append_list(list, tmp, v);

		has_next = dbus_message_iter_next(iter);
	}
	list = caml_list_rev(list);
	CAMLreturn(list);
}

static value message_get_array_array(DBusMessageIter *iter)
{
	CAMLparam0();
	CAMLlocal3(tmp, list, v);
	int has_next;

	DEBUG_GET("array_array\n");
	list = tmp = Val_emptylist;
	has_next = dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_INVALID;
	while (has_next) {
		DBusMessageIter sub;
		int element_ty;

		element_ty = dbus_message_iter_get_element_type(iter);
		dbus_message_iter_recurse(iter, &sub);
		v = message_get_array(&sub, element_ty, 1);
		caml_append_list(list, tmp, v);

		has_next = dbus_message_iter_next(iter);
	}
	list = caml_list_rev(list);
	CAMLreturn(list);
}

static value message_get_array_dict(DBusMessageIter *iter)
{
	CAMLparam0();
	CAMLlocal5(tmp, list, v, r, tuple);
	int has_next;

	DEBUG_GET("array_dict\n");
	list = tmp = Val_emptylist;
	has_next = dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_INVALID;
	while (has_next) {
		DBusMessageIter sub;
		int subtype;

		/* alloc empty tuple */
		tuple = caml_alloc_tuple(2);
		Field(tuple, 0) = Val_unit;
		Field(tuple, 1) = Val_unit;

		dbus_message_iter_recurse(iter, &sub);

		v = message_get_one(&sub, &subtype);
		caml_alloc_variant_param(r, subtype, v);
		Store_field(tuple, 0, r);

		dbus_message_iter_next(&sub);

		v = message_get_one(&sub, &subtype);
		caml_alloc_variant_param(r, subtype, v);
		Store_field(tuple, 1, r);

		caml_append_list(list, tmp, tuple);

		has_next = dbus_message_iter_next(iter);
	}
	list = caml_list_rev(list);
	CAMLreturn(list);
}


/* iter is on the values of the array */
static value message_get_array(DBusMessageIter *iter, int array_c_type, int initial_has_next)
{
	CAMLparam0();
	CAMLlocal2(v, r);
	int type;

	DEBUG_GET("array: %c (%d)\n", array_c_type, array_c_type);
	type = find_index_equal(array_c_type, __type_array_table);
	if (IS_BASIC(array_c_type)) {
		/* basic are all in the form : BASIC of list */
		v = message_get_list(iter, dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_INVALID, 0);
		caml_alloc_variant_param(r, type, v);
	} else if (array_c_type == DBUS_TYPE_DICT_ENTRY) {
		#if 0
		/* create the signature */
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
		#endif
		v = message_get_array_dict(iter);
		caml_alloc_variant_param2(r, type, Val_unit, v);

		/* XXX: the signature generated is useless, the type are self sufficient to determine the signature.
		   should be fixed someday for completeness. */
		v = caml_alloc_tuple(2);
		Field(v, 0) = Val_int(0); /* FIXME */
		Field(v, 1) = Val_int(0); /* FIXME */
		Store_field(r, 0, v);
	} else if (array_c_type == DBUS_TYPE_VARIANT) {
		v = message_get_list(iter, dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_INVALID, 1);
		caml_alloc_variant_param(r, type, v);
	} else if (array_c_type == DBUS_TYPE_STRUCT) {
		v = message_get_array_struct(iter);
		caml_alloc_variant_param2(r, type, Val_emptylist, v); /* Structs of ([], v) */
	} else if (array_c_type == DBUS_TYPE_ARRAY) {
		v = message_get_array_array(iter);
		caml_alloc_variant_param2(r, type, Val_int(0), v);
	} else {
		/*printf("array_c_type: unknown %c (%d)\n", array_c_type, array_c_type); */
		caml_alloc_variant(r, Val_int(0)); /* r = Dbus.Ty(Unknown) */
	}

	CAMLreturn(r);
}

static value message_get_struct(DBusMessageIter *iter, int initial_has_next)
{
	CAMLparam0();
	value v;

	DEBUG_GET("struct\n");
	v = message_get_list(iter, 1, 1);
	CAMLreturn(v);
}

/** dbus_ty_one_from_c returns one value beeing the raw representation of
 *  the type. meaning it's not tagged for use as an ocaml variant type.
 */
static value message_get_one(DBusMessageIter *iter, int *subtype)
{
	CAMLparam0();
	CAMLlocal1(v);
	int c_type, type;

	c_type = dbus_message_iter_get_arg_type(iter);
	DEBUG_GET("one: %c (%d)\n", c_type, c_type);
	type = find_index_equal(c_type, __type_table);
	v = Val_unit;

	if (IS_BASIC(c_type)) {
		v = message_get_basic(iter, c_type);
	} else if (c_type == DBUS_TYPE_ARRAY) {
		DBusMessageIter sub;

		dbus_message_iter_recurse(iter, &sub);
		v = message_get_array(&sub, dbus_message_iter_get_element_type(iter), 1);
	} else if (c_type == DBUS_TYPE_STRUCT) {
		DBusMessageIter sub;

		dbus_message_iter_recurse(iter, &sub);
		v = message_get_struct(&sub, 1);
	} else if (c_type == DBUS_TYPE_VARIANT) {
		DBusMessageIter sub;
		int subtype;
		value r;

		dbus_message_iter_recurse(iter, &sub);
		v = message_get_one(&sub, &subtype);
		caml_alloc_variant_param(r, subtype, v);
		v = r;
	} else {
		caml_alloc_variant(v, Val_int(0));
	}

	if (subtype)
		*subtype = type;
	CAMLreturn(v);
}

/** dbus_ty_list_from_c returns a caml list of value.
 * if alloc_variant is true, then we allocated the ocaml variant-type tag. List of Dbus.ty
 * otherwise, we allocate a raw list of values. List of string, List of int, etc
 */
static value message_get_list(DBusMessageIter *iter, int initial_has_next, int alloc_variant)
{
	CAMLparam0();
	CAMLlocal4(tmp, list, v, r);
	int has_next;

	DEBUG_GET("list: alloc_variant=%d\n", alloc_variant);
	/* initialize local caml values */
	tmp = list = Val_emptylist;
	r = Val_unit;
	v = Val_unit;

	has_next = initial_has_next;
	while (has_next) {
		int subtype;

		v = message_get_one(iter, &subtype);
		if (alloc_variant) {
			caml_alloc_variant_param(r, subtype, v);
		}
		caml_append_list(list, tmp, (alloc_variant ? r : v));

		has_next = dbus_message_iter_next(iter);
	}
	list = caml_list_rev(list);
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
	v = message_get_list(&args, has_next, 1);

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

value stub_dbus_message_marshal(value message)
{
	CAMLparam1(message);
	CAMLlocal1(msgstr);
	char *c_msgstr;
	int c_msglen;

	if (!dbus_message_marshal(DBusMessage_val(message), &c_msgstr, &c_msglen))
		caml_raise_out_of_memory();
	msgstr = caml_alloc_string(c_msglen);
	memcpy(String_val(msgstr), c_msgstr, c_msglen);

	CAMLreturn(msgstr);
}

/**************** PENDING CALL ******************/
value stub_dbus_pending_call_block(value pending)
{
	CAMLparam1(pending);
	caml_enter_blocking_section();
	dbus_pending_call_block(DBusPendingCall_val(pending));
	caml_leave_blocking_section();
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
		raise_dbus_internal_error("dbus_pending_call_steal_reply");
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
		switch (Int_val(Field(flags, 0))) {
		case 0: c_flags |= DBUS_WATCH_READABLE; break;
		case 1: c_flags |= DBUS_WATCH_WRITABLE; break;
		default: /* ouphfm */ break;
		}
	}
	dbus_watch_handle(DBusWatch_val(watch), c_flags);

	CAMLreturn(Val_unit);
}

value stub_dbus_timeout_get_interval(value timeout)
{
	CAMLparam1(timeout);
	int ret;

	ret = dbus_timeout_get_interval(DBusTimeout_val(timeout));
	CAMLreturn(Val_int(ret));
}

value stub_dbus_timeout_handle(value timeout)
{
	CAMLparam1(timeout);
	int ret;

	ret = dbus_timeout_handle(DBusTimeout_val(timeout));
	CAMLreturn(Val_bool(ret));
}

value stub_dbus_timeout_get_enabled(value timeout)
{
	CAMLparam1(timeout);
	int ret;

	ret = dbus_timeout_get_enabled(DBusTimeout_val(timeout));
	CAMLreturn(Val_bool(ret));
}
