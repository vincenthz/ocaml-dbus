let nm_interface = "org.freedesktop.NetworkManager"
let nm_interface_device = "org.freedesktop.NetworkManager.Device"
let nm_name = nm_interface
let nm_path = "/org/freedesktop/NetworkManager"

let print_dbus_ty_list l =
	List.iter (fun o -> Printf.printf "%s\n" (DBus.string_of_ty o)) l

let send_msg_and_print ~bus ~destination ~path ~intf ~serv ~params =
	let msg = DBus.Message.new_method_call destination path intf serv in
	DBus.Message.append msg params;
	let r = DBus.Connection.send_with_reply_and_block bus msg (-1) in
	let ty = DBus.Message.get_type r in
	let l = DBus.Message.get r in
	l

let send_nm_msg_and_print = send_msg_and_print ~destination:nm_name ~path:nm_path ~intf:nm_interface
let send_nm_device_msg_and_print ~dev ~intf = send_msg_and_print ~destination:nm_name ~path:dev ~intf

let () =
	let bus = DBus.Bus.get DBus.Bus.System in
	let devices = send_nm_msg_and_print ~bus ~serv:"GetDevices" ~params:[] in
	match devices with
	| [ DBus.Array (DBus.ObjectPaths devs) ] ->
		List.iter (fun dev ->
			Printf.printf "device: %s\n" dev;
			let intf = "org.freedesktop.DBus.Properties" in
			let params = [
				DBus.String "org.freedesktop.NetworkManager.Device";
				DBus.String "HwAddress";
			] in
			let ret = send_nm_device_msg_and_print ~bus ~dev ~intf ~serv:"Get" ~params in
			print_dbus_ty_list ret;
		) devs;
		()
	| _ ->
		Printf.eprintf "unexpected reply from GetDevices";
		exit 1
