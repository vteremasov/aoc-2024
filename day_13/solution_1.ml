let file = "./day_13/input/input";;

let determinant a b c d = a *. d -. b *. c;;

let solve_by_cramer (a1, b1, c1) (a2, b2, c2) =
  let d = determinant a1 b1 a2 b2 in
  if d = 0.0 then
    None
  else
    let dx = determinant c1 b1 c2 b2 in
    let dy = determinant a1 c1 a2 c2 in
    Some (dx /. d, dy /. d)
;;

let splitrim str chr =
  String.split_on_char chr str
  |> List.map String.trim
  |> List.filter (fun s -> (String.length s) > 0)
;;

let extract_number fn str =
  let regex = Str.regexp "\\([0-9]+\\)" in
  if Str.string_match regex str 0 then
    fn (Str.matched_group 1 str)
  else
    fn "0"
;;

let get_buttons line =
  let words = splitrim line ' ' in
  let but_x = List.nth (splitrim (List.nth words 2) '+') 1 in
  let but_y = List.nth (splitrim (List.nth words 3) '+') 1 in
  (extract_number float_of_string but_x, extract_number float_of_string but_y)
;;

let get_prize line =
  let words = splitrim line ' ' in
  let p_x = List.nth (splitrim (List.nth words 1) '=') 1 in
  let p_y = List.nth (splitrim (List.nth words 2) '=') 1 in
  (extract_number float_of_string p_x, extract_number float_of_string p_y)
;;

let get_parsed lst =
  List.map (fun str ->
      let lines = splitrim str '\n' in
      (get_buttons (List.hd lines), get_buttons (List.nth lines 1), get_prize (List.nth lines 2))
    ) lst
;;

let is_act_int num =
  num = float_of_int (int_of_float num)
;;

let prepare f =
  In_channel.with_open_bin f In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> List.map String.trim
  |> get_parsed
  |> List.map (fun ((ax, ay), (bx, by), (px, py)) -> solve_by_cramer (ax, bx, px) (ay, by, py))
  |> List.filter_map (Fun.id)
  |> List.filter (fun (a, b) -> is_act_int a && is_act_int b)
  |> List.map (fun (a, b) -> (int_of_float a, int_of_float b))
  |> List.fold_left (fun acc (a, b) -> acc + ((a * 3) + (b * 1))) 0
;;

let result () = prepare file;

