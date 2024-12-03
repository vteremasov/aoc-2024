let input_file = "./day_2/input/input2.txt"

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

let sort_asc lst =
  List.stable_sort (fun a b -> if a >= b then 1 else 0) lst;;

let trim_parse lst =
  List.map int_of_string (List.filter_map is_empty (List.map String.trim lst));;

let file_content = read_lines input_file;;
let file_content_splitted = List.map (String.split_on_char ' ') (file_content);;
let file_content_parsed = List.map trim_parse file_content_splitted;;

let rec is_inc lst =
  match lst with
  | [] | [_] -> true
  | x :: y :: rest -> if x < y then is_inc (y :: rest) else false;;

let rec is_dec lst =
  match lst with
  | [] | [_] -> true
  | x :: y :: rest -> if x > y then is_dec (y :: rest) else false;;

let rec is_close lst =
  match lst with
  | [] | [_] -> true
  | hd1 :: hd2 :: rest -> if abs(hd1 - hd2) <= 3 then is_close (hd2 :: rest) else false;;

let is_safe lst =
  ((is_inc lst || is_dec lst) && is_close lst) && List.length lst > 0;;

let is_safe_2 lst =
  let rec check l1 l2 =
    match l2 with
    | [] -> false
    | x::rest2 -> is_safe (l1 @ rest2) || check (l1@[x]) rest2
  in
  check [] lst;;

let result () = List.fold_left (fun acc el -> if is_safe_2 el then acc + 1 else acc) 0 file_content_parsed;;

