let input_file = "./day_1/input/input1.txt"

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

let file_content = read_lines input_file;;
let file_content_plitted = List.map (String.split_on_char ' ') (file_content);;
let left = List.stable_sort (fun a b -> if a >= b then 1 else 0) (
  List.map (int_of_string) (
    List.filter_map (is_empty) (
      List.map (String.trim) (List.map List.hd (file_content_plitted))
    )
  )
);;
let right = List.stable_sort (fun a b -> if a >= b then 1 else 0) (
  List.map (int_of_string) (
    List.filter_map (is_empty) (
      List.map List.hd (List.map List.rev (file_content_plitted))
    )
  )
);;

let rec count_dst lft rgt =
  match lft, rgt with
  | _, [] -> []
  | [], _ -> []
  | hlft :: tlft, hrgt :: trgt -> abs (hlft - hrgt) :: count_dst tlft trgt;;

let result () = List.fold_left (+) 0 (count_dst left right);;

