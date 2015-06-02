open Unix
 
let init_socket addr port =
  let inet_addr = (gethostbyname addr).h_addr_list.(0) in
  let sockaddr = ADDR_INET (inet_addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock sockaddr;
  (* convert the file descriptor into high-level channels: *)
  let outchan = out_channel_of_descr sock in
  let inchan = in_channel_of_descr sock in
  (inchan, outchan)
let () =
  let ic, oc = init_socket "10.0.196.97" 20002 in
  (* print_string "hello socket world\n" *)
  output_string oc "HELLO FOO\n";
  flush oc;
  try while true do
      let line = input_line ic in
      Printf.printf "%s\n%!" line;
    done
  with End_of_file -> 
    print_endline ":(";
    close_in ic
;;
