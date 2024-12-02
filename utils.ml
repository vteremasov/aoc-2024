let rec print_str_list lst =
  match lst with
  | [] -> ()
  | h :: t -> print_string h; print_string " "; print_str_list t

let rec print_int_list lst =
  match lst with
  | [] -> ()
  | h :: t -> print_int h; print_string " "; print_int_list t

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents;;

let is_empty str =
  match str with
  | "" -> None
  | x -> Some(x)

let filter_empty_lines lst =
  List.filter (fun x -> x != "");;

let trim lst = 
  List.map String.trim;;

let sort_asc lst =
  List.stable_sort (fun a b -> if a >= b then 1 else 0) lst;;

