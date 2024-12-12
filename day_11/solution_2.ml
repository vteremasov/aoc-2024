open List  ;;

let file = "./day_11/input/input";;

let safe_add x y =
  if x > 0 && y > max_int - x then
    failwith "Overflow"
  else if x < 0 && y < min_int - x then
    failwith "Underflow"
  else
    x + y
;;

let split_string_half s =
  let len = String.length s in
  let mid = len / 2 in
  let first_half = String.sub s 0 mid in
  let second_half = String.sub s mid (len - mid) in
  (first_half, second_half)
;;

let apply_rules el =
  if el = 0 then
    [1]
  else if (String.length(string_of_int el)) mod 2 = 0 then
    let (f, s) = split_string_half (string_of_int el) in
    [(int_of_string s); (int_of_string f)]
  else
    [el * 2024]
;;

let cache = Hashtbl.create 75;;

let key a b = (string_of_int a) ^ "-" ^ (string_of_int b)

let rec count num iteration =
  if iteration = 0 then
    1
  else
    List.map (fun n -> 
      if Hashtbl.mem cache (key iteration n) then
        Hashtbl.find cache (key iteration n)
      else
        let r = count n (iteration - 1) in
        Hashtbl.add cache (key iteration n) r;
        r
    ) (apply_rules num)
    |> List.fold_left (+) 0
;;

let change_times2 times lst =
  List.fold_left (fun acc num -> safe_add acc (count num times)) 0 lst
;;

let solve f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.trim
  |> String.split_on_char ' '
  |> filter (fun str -> String.length (String.trim str) > 0)
  |> map int_of_string
  |> change_times2 75
;;

let result () = solve file;

