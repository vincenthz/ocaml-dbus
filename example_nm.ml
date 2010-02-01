let nm_interface = "org.freedesktop.NetworkManager"
let nm_name = nm_interface
let nm_path = "/org/freedesktop/NetworkManager"

let () =
	let bus = DBus.Bus.get DBus.Bus.System in
	let msg = DBus.Message.new_method_call nm_name nm_path nm_interface "GetDevices" in
	let r = DBus.Connection.send_with_reply_and_block bus msg (-1) in
	let l = DBus.Message.get r in
	List.iter (fun o ->
		Printf.printf "%s\n" (DBus.string_of_ty o)
	) l
