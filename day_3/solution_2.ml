let input_file = "./day_3/input/input2.txt"

let rec print_str_list lst =
  match lst with
  | [] -> ()
  | h :: t -> print_string h; print_string "\n\n"; print_str_list t

let rec print_int_list lst =
  match lst with
  | [] -> ()
  | h :: t -> print_int h; print_string " "; print_int_list t

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  contents;;

let file_content = read_lines input_file;;

let parse_mul_str str =
  if String.length str >= 4 && String.starts_with ~prefix:"mul(" str then
    match String.index_opt str ')' with
    | Some paren_close_idx -> (
        let args_str = String.sub str 4 (paren_close_idx - 4) in
        match String.split_on_char ',' args_str with
        | [ arg1; arg2 ] -> Some (arg1, arg2)
        | _ -> None)
    | None -> None
  else None

let convert_to_int_pair opt_pair =
  match opt_pair with
  | Some (arg1, arg2) -> (
      try Some (int_of_string arg1, int_of_string arg2) with Failure _ -> None)
  | None -> None

let parse_mul str = parse_mul_str str |> convert_to_int_pair

let rec parse_muls str flag =
  match str with
  | "" -> []
  | _ -> 
      if flag then
        if String.starts_with ~prefix:"don't()" str then None :: parse_muls (String.sub str 1 (String.length str - 1)) false
        else parse_mul str :: parse_muls (String.sub str 1 (String.length str - 1)) true
      else 
        if String.starts_with ~prefix:"do()" str then parse_mul str :: parse_muls (String.sub str 1 (String.length str - 1)) true
        else None :: parse_muls (String.sub str 1 (String.length str - 1)) false;;

let parsed_muls = List.filter_map (fun x -> x) (parse_muls (read_lines input_file) true);;

let result = List.fold_left (+) 0 (List.map (fun (x, y) -> x * y) parsed_muls);;

Printf.printf "result_2: %d\n" result;; 

