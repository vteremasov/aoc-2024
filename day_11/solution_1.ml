open List  ;;

let file = "./day_11/input/input";;

let split_string_half s =
  let len = String.length s in
  let mid = len / 2 in
  let first_half = String.sub s 0 mid in
  let second_half = String.sub s mid (len - mid) in
  (first_half, second_half)
;;

let apply_rules acc el =
  if el = 0 then
    1::acc
  else if (String.length(string_of_int el)) mod 2 = 0 then
    let (f, s) = split_string_half (string_of_int el) in
    (int_of_string s)::(int_of_string f)::acc
  else
    (el * 2024)::acc
;;

let change lst =
  List.fold_left (apply_rules) [] lst
;;

let change_times times lst =
  Seq.forever (Fun.id)
  |> Seq.take times
  |> Seq.fold_left (fun acc () -> change acc) lst
;;

let solve f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.trim
  |> String.split_on_char ' '
  |> filter (fun str -> String.length (String.trim str) > 0)
  |> map int_of_string
  |> change_times 25
  |> List.length
;;

let result () = solve file;

