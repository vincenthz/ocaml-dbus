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
	(*print_dbus_ty_list (DBus.Message.get msg);*)
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
	let bus = DBus.Bus.get DBus.Bus.Session in
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
(****************** Test Packets *********************************************)
(*****************************************************************************)
let test () =
	let msg = DBus.Message.new_method_call notif_name notif_path notif_interface "X" in
	let params = [
		DBus.String "abc";
		DBus.Array (DBus.Strings [ "abc"; "def" ]);
		DBus.Variant (DBus.Int32 1l);
		DBus.Struct [ DBus.String "abc"; DBus.Int64 10L ];
		DBus.Array (DBus.ObjectPaths [ "/abc"; "/def"; ]);
		DBus.Array (DBus.Variants [ DBus.String "abc"; DBus.Int32 400l ]);
		DBus.Array (DBus.Arrays (DBus.SigString, [ DBus.Strings [ "x" ]; DBus.Strings [ "y"; "z" ] ]));
		DBus.Array (DBus.Arrays (DBus.SigInt64, [ DBus.Int64s [ 10L; 24L ]; DBus.Int64s [ 54L; 12L ]; ]));
		DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigString), [ DBus.String "abc", DBus.String "def"; DBus.String "2k", DBus.String "2v" ]));
		DBus.Array (DBus.Structs ([ DBus.SigString; DBus.SigString; DBus.SigInt32 ],
			[
				[ DBus.String "abc"; DBus.String "def"; DBus.Int32 10l ];
				[ DBus.String "xxx"; DBus.String "yzy"; DBus.Int32 2901l ];
			]))
	] in
	DBus.Message.append msg params;
	print_dbus_ty_list (DBus.Message.get msg);

	let msg = DBus.Message.new_method_call notif_name notif_path notif_interface "X" in
	let params = [
		DBus.Array (DBus.Strings [ "abc" ]);
		DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigString), [ (DBus.String "key", DBus.String "value") ]));
		DBus.Array (DBus.Variants [ DBus.String "abc" ]);
		DBus.Variant (DBus.Array (DBus.Strings [ "abc" ]));
		DBus.Variant (DBus.Struct [ DBus.String "abc"; DBus.Int64 10L ]);
		DBus.Array (DBus.Structs ([ DBus.SigString ], [ [ DBus.String "x" ]; [ DBus.String "y" ] ]));
		DBus.Array (DBus.Dicts ((DBus.SigString, DBus.SigArray DBus.SigString),
		                        [ (DBus.String "x", DBus.Array (DBus.Strings [ "abc"; "def" ]) ) ]
		));
	] in
	DBus.Message.append msg params;
	print_dbus_ty_list (DBus.Message.get msg);
	()

(*****************************************************************************)
(****************** Request Name *********************************************)
(*****************************************************************************)
let service () =
	let bus = DBus.Bus.get DBus.Bus.Session in
	let serv = "org.test.dbus.ocaml.bindings" in
	let rep = DBus.Bus.request_name bus serv [ DBus.Bus.DoNotQueue ] in
	let repstr = match rep with
		| DBus.Bus.PrimaryOwner -> "primary ownwer"
		| DBus.Bus.InQueue -> "in queue"
		| DBus.Bus.Exists -> "exists"
		| DBus.Bus.AlreadyOwner -> "already owner"
		| DBus.Bus.ReqUnknown i -> Printf.sprintf "unknown %d" i
		in
	Printf.printf "grabbing %s : %s\n" serv repstr;

	let rep = DBus.Bus.release_name bus serv in
	let repstr = match rep with
		| DBus.Bus.Released -> "released"
		| DBus.Bus.NonExistent -> "non existent"
		| DBus.Bus.NotOwner -> "not owner"
		| DBus.Bus.RelUnknown i -> Printf.sprintf "unknown %d" i
		in
	Printf.printf "releasing %s : %s\n" serv repstr;
	()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
let () =
	match Sys.argv.(1) with
	| "nm"           -> example_nm ();
	| "notification" -> example_notification ();
	| "avahi"        -> ()
	| "test"         -> test ()
	| "service"      -> service ()
	| _              -> ()
