let rec print_lst_str sep lst = 
  match lst with
  | [] -> Printf.printf "%c" sep
  | hd::rest -> Printf.printf "%s %c" hd sep; print_lst_str sep rest
;;

let print_str_rep str x =
  String.iteri (fun i c -> if i = x then Printf.printf "%c" '#' else Printf.printf "%c" c) str;
  Printf.printf "\n";
;;

let print_lst_str_rep _ lst (x, y) = 
  List.iteri (fun i str -> if i = y then print_str_rep str x else Printf.printf "%s \n" str) lst
;;

let nop e = e;;

let rec add_uniq_v2 lst (x, y) =
  match lst with
  | [] -> [(x, y)]
  | (xb, yb)::rest -> if xb = x && yb = y then ((xb, yb) :: rest) else ((xb, yb) :: add_uniq_v2 rest (x, y))
;;

let is_end map (x, y) =
  x < 0 || y < 0 || List.length map <= y || String.length (List.hd map) <= x
;;

let char_to_int c = int_of_char c - int_of_char '0';;

