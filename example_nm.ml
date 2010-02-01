let nm_interface = "org.freedesktop.NetworkManager"
let nm_interface_device = "org.freedesktop.NetworkManager.Device"
let nm_name = nm_interface
let nm_path = "/org/freedesktop/NetworkManager"

let send_msg_and_print ~bus ~name ~path ~intf ~serv ~params =
	let msg = DBus.Message.new_method_call name path intf serv in
	DBus.Message.append msg params;
	let r = DBus.Connection.send_with_reply_and_block bus msg (-1) in
	let l = DBus.Message.get r in
	List.iter (fun o ->
		Printf.printf "%s\n" (DBus.string_of_ty o)
	) l;
	l

let send_nm_msg_and_print = send_msg_and_print ~name:nm_name ~path:nm_path ~intf:nm_interface
let send_nm_device_msg_and_print ~dev = send_msg_and_print ~name:nm_name ~path:dev ~intf:nm_interface_device

let () =
	let bus = DBus.Bus.get DBus.Bus.System in
	let devices = send_nm_msg_and_print ~bus ~serv:"GetDevices" ~params:[] in
	match devices with
	| [ DBus.Array (DBus.ObjectPaths devs) ] ->
		(*
		List.iter (fun dev ->
			let ret = send_nm_device_msg_and_print ~bus ~dev ~serv:"getInterface" ~params:[] in
			()
		) devs
		*)
		()
	| _ ->
		Printf.eprintf "unexpected reply from GetDevices";
		exit 1
