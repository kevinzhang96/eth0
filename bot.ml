open Unix
open Array
open Str

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
  let ic, oc = init_socket "localhost" 20000 in 
  output_string oc "HELLO MOOOOOOATHER\n";
  flush oc;
  let orders = Array.make 10 ("FOO", 0, false) in 
  Array.set orders 2 ("BAR", 0, false);
  Array.set orders 3 ("BAR", 0, false);
  Array.set orders 4 ("BAZ", 0, false);
  Array.set orders 5 ("BAZ", 0, false);
  Array.set orders 6 ("CORGE", 0, false);
  Array.set orders 7 ("CORGE", 0, false);
  Array.set orders 8 ("QUUX", 0, false);
  Array.set orders 9 ("QUUX", 0, false);
  let running = ref true in 
  let symforindex (i: int) =
    (if i = 1 || i = 2 then "FOO"
    else if i = 3 || i = 4 then "BAR"
    else if i = 5 || i = 6 then "BAZ"
    else if i = 7 || i = 8 then "CORGE"
    else "QUUX") in 
  let buy (p: int) (c: int) (s: string) =
    let id = 
    (match s with
    | "FOO" -> 0
    | "BAR" -> 2
    | "BAZ" -> 4
    | "CORGE" -> 6
    | "QUUX" -> 8) in
    let (s2, p2, b2) = Array.get orders (id+1) in
    if p2 > p then let (s3, p3, b3) = Array.get orders id in if not b3 || p3 > p then 
      Array.set orders id (s, p, true);
      (* output_string oc ("CANCEL " ^ (string_of_int id) ^ "\n"); *)
      output_string oc ("ADD " ^ (string_of_int id) ^ " " ^ s ^ " BUY " ^ (string_of_int p) 
        ^ " " ^ (string_of_int c) ^ "\n");
      flush oc in 
  let sell (p: int) (c: int) (s: string) = 
    let id = 
    (match s with
    | "FOO" -> 1
    | "BAR" -> 3
    | "BAZ" -> 5
    | "CORGE" -> 7
    | "QUUX" -> 9) in
    let (s2, p2, b2) = Array.get orders (id-1) in
    if (p2 < p) then let (s3, p3, b3) = Array.get orders id in if not b3 || p3 < p then 
      Array.set orders id (s, p, true);
      (* output_string oc ("CANCEL " ^ (string_of_int id) ^ "\n"); *)
      output_string oc ("ADD " ^ (string_of_int id) ^ " " ^ s ^ " SELL " ^ (string_of_int p) 
        ^ " " ^ (string_of_int c) ^ "\n");
      flush oc in 
  let cancel (o: int) =   
    Array.set orders o (symforindex o, 0, false); 
    output_string oc ("CANCEL " ^ (string_of_int o) ^ "\n");
    flush oc in 
  let rec prices (ts: string list) (curr: int * int) : 
    (int * int) = 
    (match ts with
    | hd::tl -> if hd = "BUY" then (match tl with
      | hd2::tl2 -> if hd2 = "SELL" then prices tl curr
        else prices tl2 (int_of_string (List.hd (Str.split (Str.regexp ":") hd2)), snd curr)
      | [] -> curr)
      else if hd = "SELL" then (match tl with
      | hd2::tl2 -> (fst curr, int_of_string (List.hd (Str.split (Str.regexp ":") hd2)))
      | [] -> curr) else prices tl curr
    | [] -> curr) in   
  let book (ts: string list) =
    let s = List.nth ts 1 in
    let (hb, ls) = prices ts (0,0) in 
    if (ls - hb > 1) then buy (hb + 1) 100 s; sell (ls - 1) 100 s in 
  let handle (l: string) = 
    let tokens = Str.split (Str.regexp " ") l in
    (match List.nth tokens 0 with
    | "MARKET_OPEN" -> running := true; "Market open"
    | "MARKET_CLOSED" -> running := false; "Market closed"
    | "ERROR" -> List.nth tokens 1 ^ "\n"
    | "BOOK" -> Printf.printf "%s\n%!" (List.nth tokens 1); book tokens; "Handling book transaction"
    | "TRADE" -> "Traded " ^ (List.nth tokens 1) ^ ", " ^ (List.nth tokens 3) ^ " at " ^ (List.nth tokens 2) ^ "\n"
    | "ACK" -> "Trade accepted\n"
    | "REJECT" -> "Order " ^ (List.nth tokens 1) ^ " was rejected: " 
        ^ (List.nth tokens 2) ^ "\n"
    | "FILL" -> "Order " ^ (List.nth tokens 1) ^ " was filled.\n\t" ^ (List.nth tokens 2) ^ 
        " " ^ (List.nth tokens 3) ^ "\n\t" ^ (List.nth tokens 4) ^ " at " ^ (List.nth tokens 5) ^ "\n"
    | "OUT" -> "Order " ^ (List.nth tokens 1) ^ " is out.\n"
    | "HELLO" -> "HELLODER") in
  try while true do
      let line = input_line ic in
      Printf.printf "%s\n%!" (handle line);
      ignore (Unix.select [] [] [] 1.5);
    done  
  with End_of_file -> 
    print_endline ":(";
    close_in ic
;;