let file = "./day_7/input/input";;

let is_empty s =
  String.length (String.trim s) > 0
;;

let concat a b =
  int_of_string (string_of_int a ^ string_of_int b)
;;

let rec comb lst =
  match lst with
  | [] -> []
  | [e] -> [e]
  | f::s::rest -> comb (f+s :: rest) @ comb (f*s :: rest) @ comb ((concat f s) :: rest)
;;

let prepare f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter is_empty
  |> List.map (fun s -> String.split_on_char ':' s)
  |> List.map (fun lst -> (List.hd lst, List.nth lst 1))
  |> List.map (fun (test, data) -> (int_of_string test, String.split_on_char ' ' data |> List.filter is_empty |> List.map int_of_string))
  |> List.map (fun (test, data) -> (test, comb data))
  |> List.filter (fun (test, combs) -> List.exists (fun c -> c = test) combs)
  |> List.map (fun (test, _) -> test)
  |> List.fold_left (+) 0
;;

let result () = prepare file;

