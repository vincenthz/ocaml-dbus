(* Browse the local network for ssh services using Avahi and D-Bus.
 * There is *zero* documentation for this.  I examined a lot of code
 * to do this, and the following page was also very helpful:
 * http://www.amk.ca/diary/2007/04/rough_notes_python_and_dbus.html
 * See also the DBus API reference:
 * http://dbus.freedesktop.org/doc/dbus/api/html/index.html
 * See also Dan Berrange's Perl bindings:
 * http://search.cpan.org/src/DANBERR/Net-DBus-0.33.5/lib/Net/
 *
 * By Richard W.M. Jones <rich@annexia.org> or <rjones@redhat.com>.
 * PUBLIC DOMAIN example code.
 *)

open Printf
open DBus

let debug = true
let service = "_ssh._tcp"

let rec print_msg msg =
  (match Message.get_type msg with
   | Message.Invalid ->
       printf "Invalid";
   | Message.Method_call ->
       printf "Method_call";
   | Message.Method_return ->
       printf "Method_return";
   | Message.Error ->
       printf "Error";
   | Message.Signal ->
       printf "Signal");

  let print_opt f name =
    match f msg with
    | None -> ()
    | Some value -> printf " %s=%S" name value
  in
  print_opt Message.get_member "member";
  print_opt Message.get_path "path";
  print_opt Message.get_interface "interface";
  print_opt Message.get_sender "sender";

  let fields = Message.get msg in
  printf "(";
  print_fields fields;
  printf ")\n%!";

and print_fields fields =
  printf "%s" (String.concat ", " (List.map string_of_ty fields))

(* Perform a synchronous call to an object method. *)
let call_method ~bus ~err ~name ~path ~interface ~methd args =
  (* Create the method_call message. *)
  let msg = Message.new_method_call name path interface methd in
  Message.append msg args;
  (* Send the message, get reply. *)
  let r = Connection.send_with_reply_and_block bus msg (-1) err in
  Message.get r

(* A service has appeared on the network.  Resolve its IP address, etc. *)
let resolve_service bus err sb_path msg =
  let fields = Message.get msg in
  match fields with
    (* match fields in the ItemNew message from ServiceBrowser. *)
  | [(Int32 _) as interface;
     (Int32 _) as protocol;
     (String _) as name;
     (String _) as service;
     (String _) as domain;
     _ (* flags *)] ->
      (* Create a new ServiceResolver object which is used to resolve
       * the actual locations of network services found by the ServiceBrowser.
       *)
      let sr =
	call_method ~bus ~err
	  ~name:"org.freedesktop.Avahi"
	  ~path:"/"
	  ~interface:"org.freedesktop.Avahi.Server"
	  ~methd:"ServiceResolverNew"
	  [
	    interface;
	    protocol;
	    name;
	    service;
	    domain;
	    Int32 (-1_l);		(* AVAHI_PROTO_UNSPEC *)
	    UInt32 0_l;			(* flags *)
	  ] in
      let sr_path =
	match sr with
	| [ ObjectPath path ] -> path
	| _ -> assert false in

      if debug then eprintf "ServiceResolver path = %S\n%!" sr_path;

      (* Add a match rule so we see these all signals of interest. *)
      Bus.add_match bus
	(String.concat "," [
	   "type='signal'";
	   "sender='org.freedesktop.Avahi.ServiceResolver'";
	   "path='" ^ sr_path ^ "'";
	 ]) err;

      ()

  | _ ->
      prerr_endline "warning: unexpected message contents of ItemNew signal";
      ()

(* This is called when we get a message/signal.  Could be from the
 * (global) ServiceBrowser or any of the ServiceResolver objects.
 *)
let got_message bus err sb_path msg =
  if debug then print_msg msg;

  let typ = Message.get_type msg in
  let member = match Message.get_member msg with None -> "" | Some m -> m in
  let interface =
    match Message.get_interface msg with None -> "" | Some m -> m in

  if typ = Message.Signal then (
    match interface, member with
    | "org.freedesktop.Avahi.ServiceBrowser", "CacheExhausted" -> ()
    | "org.freedesktop.Avahi.ServiceBrowser", "ItemNew" ->
	(* New service has appeared, start to resolve it. *)
	resolve_service bus err sb_path msg
    | "org.freedesktop.Avahi.ServiceBrowser", "ItemRemove" ->
	(* XXX Service has disappeared. *)
	()
    | "org.freedesktop.Avahi.ServiceBrowser", "AllForNow" -> ()
    | "org.freedesktop.Avahi.ServiceResolver", "Found" ->
	(* Resolver has resolved the name of a previously appearing service. *)
	(* XXX *)
	()
    | "org.freedesktop.DBus", _ -> ()
    | interface, member ->
	eprintf "warning: ignored unknown message %s from %s\n%!"
	  member interface
  );
  true

(* Store the connection ((bus, err) tuple).  However don't bother
 * connecting to D-Bus at all until the user opens the connection
 * dialog for the first time.
 *)
let connection = ref None

(* Create global error and system bus object, and create the service
 * browser.  XXX Probably not robust if the daemon restarts.
 *)
let connect () =
  match !connection with
  | Some (bus, err) -> (bus, err, false)
  | None ->
      let err = Error.init () in
      let bus = Bus.get Bus.System err in
      if Error.is_set err then failwith "error set after getting System bus";

      (* Create a new ServiceBrowser object which emits a signal whenever
       * a new network service of the type specified is found on the network.
       *)
      let sb =
	call_method ~bus ~err
	  ~name:"org.freedesktop.Avahi"
	  ~path:"/"
	  ~interface:"org.freedesktop.Avahi.Server"
	  ~methd:"ServiceBrowserNew"
	  [
	    Int32 (-1_l);	        (* interface, -1=AVAHI_IF_UNSPEC *)
	    Int32 0_l;			(* 0=IPv4, 1=IPv6 *)
	    String service;		(* service type *)
	    String "";			(* XXX call GetDomainName() *)
	    UInt32 0_l;			(* flags *)
	  ] in
      let sb_path =
	match sb with
	| [ ObjectPath path ] -> path
	| _ -> assert false in

      if debug then eprintf "ServiceBrowser path = %S\n%!" sb_path;

      (* Register a callback to accept the signals. *)
      (* XXX This leaks memory because it is never freed. *)
      Connection.add_filter bus (
	fun bus msg -> got_message bus err sb_path msg
      );

      (* Add a match rule so we see these all signals of interest. *)
      Bus.add_match bus
	(String.concat "," [
	   "type='signal'";
	   "sender='org.freedesktop.Avahi.ServiceBrowser'";
	   "path='" ^ sb_path ^ "'";
	 ]) err;

      connection := Some (bus, err);
      (bus, err, true)

let () =
  let bus, err, just_connected = connect () in

  (* Wait for incoming signals. *)
  while Connection.read_write_dispatch bus (-1) do ()
  done
