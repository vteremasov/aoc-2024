let input_file = "./day_1/input/example.txt"

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

let left = sort_asc (
  List.map (int_of_string) (
    List.filter_map (is_empty) (
      List.map (String.trim) (List.map List.hd (List.map (String.split_on_char ' ') (read_lines input_file)))
    )
  )
);;
let right = sort_asc (
  List.map (int_of_string) (
    List.filter_map (is_empty) (
      List.map List.hd (List.map List.rev (List.map (String.split_on_char ' ') (read_lines input_file)))
    )
  )
);;

let rec count_score lft rgt =
  match lft with
  | [] -> []
  | h::tflt -> List.length (List.filter (fun re -> re = h) rgt) * h:: count_score tflt rgt;;

let result () = List.fold_left (+) 0 (count_score left right);;

