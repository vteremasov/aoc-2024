let file = "./day_6/input/input.txt";;

let is_end lst (x, y) =
  (x < 0 || y < 0) || (List.length lst) <= y || String.length (List.hd lst) <= x
;;

let should_turn lst (x, y) c =
  (List.nth lst y).[x] = c
;;

let turn = function 
  | (0, -1) -> (1, 0)
  | (1, 0) -> (0, 1)
  | (0, 1) -> (-1, 0)
  | (-1, 0) -> (0, -1)
  | _ -> raise Not_found
;;

let find d c =
  let found = ref false in
  List.fold_left 
  (fun (x, y) str -> try let r = String.index str c in found := true; (r, y+1) with | Not_found -> if !found then (x, y) else (x, y+1)) 
  (0, 0)
  d
;;

let rec add_uniq lst (x, y) =
  match lst with
  | [] -> [(x, y)]
  | (xb, yb)::rest -> if xb = x && yb = y then ((xb, yb) :: rest) else ((xb, yb) :: add_uniq rest (x, y))
;;

let rec go res (dx, dy) (x, y) lst =
  if is_end lst (x + dx, y + dy) then
    res
  else if should_turn lst (x + dx, y + dy) '#' then
    go res (turn (dx, dy)) (x, y) lst
  else
    add_uniq (go res (dx, dy) (x + dx, y + dy) lst) (x, y)
;;

let prep f = 
  In_channel.with_open_bin f In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
;; 

let content = prep file;;
let find_symb = find content;;

let result () = List.length (go [] (0, -1) (find_symb '^') content);; 

