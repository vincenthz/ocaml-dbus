let nm_interface = "org.freedesktop.NetworkManager"
let nm_interface_device = "org.freedesktop.NetworkManager.Device"
let nm_name = nm_interface
let nm_path = "/org/freedesktop/NetworkManager"

let notif_interface = "org.freedesktop.Notifications"
let notif_name = notif_interface
let notif_path = "/org/freedesktop/Notifications"

let print_dbus_ty_list l =
	List.iter (fun o -> Printf.printf "%s\n" (DBus.string_of_ty o)) l

let send_msg ~bus ~destination ~path ~intf ~serv ~params =
	let msg = DBus.Message.new_method_call destination path intf serv in
	DBus.Message.append msg params;
	print_dbus_ty_list (DBus.Message.get msg);
	let r = DBus.Connection.send_with_reply_and_block bus msg (-1) in
	let l = DBus.Message.get r in
	l

(*****************************************************************************)
(****************** NetworkManager daemon ************************************)
(*****************************************************************************)
let send_nm_msg = send_msg ~destination:nm_name ~path:nm_path ~intf:nm_interface
let send_nm_device_msg ~dev ~intf = send_msg ~destination:nm_name ~path:dev ~intf

let example_nm () =
	let bus = DBus.Bus.get DBus.Bus.System in
	let devices = send_nm_msg ~bus ~serv:"GetDevices" ~params:[] in
	match devices with
	| [ DBus.Array (DBus.ObjectPaths devs) ] ->
		List.iter (fun dev ->
			Printf.printf "device: %s\n" dev;
			let intf = "org.freedesktop.DBus.Properties" in
			let params1 = [
				DBus.String "org.freedesktop.NetworkManager.Device";
				DBus.String "HwAddress";
			] in
			let params2 = [
				DBus.String "org.freedesktop.NetworkManager.Device";
				DBus.String "Interface";
			] in
			let ret1 = send_nm_device_msg ~bus ~dev ~intf ~serv:"Get" ~params:params1 in
			let ret2 = send_nm_device_msg ~bus ~dev ~intf ~serv:"Get" ~params:params2 in
			print_dbus_ty_list ret1;
			print_dbus_ty_list ret2;
		) devs;
		()
	| _ ->
		Printf.eprintf "unexpected reply from GetDevices";
		exit 1

(*****************************************************************************)
(****************** Notifications daemon *************************************)
(*****************************************************************************)
let send_notif_msg = send_msg ~destination:notif_name ~path:notif_path ~intf:notif_interface

let example_notification () =
	let bus = DBus.Bus.get DBus.Bus.System in
	let params = [
		DBus.String "y";
		DBus.UInt32 1l;
		DBus.String "x";
		DBus.String "z";
		DBus.String "w";
		DBus.Array (DBus.Strings []);
		DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigVariant), []));
		DBus.Int32 4000l;
	] in	
	let r = send_notif_msg ~bus ~serv:"Notify" ~params in
	print_dbus_ty_list r;
	()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
let () =
	match Sys.argv.(1) with
	| "nm"           -> example_nm ();
	| "notification" -> example_notification ();
	| "avahi"        -> ()
	| _              -> ()
