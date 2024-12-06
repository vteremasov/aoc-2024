let file = "./day_6/input/example.txt";;

let width s =
  (String.index s '\n') - 1
;;

let height s =
  List.length(String.split_on_char '\n' s) / (width s)
;;

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

type tuple =
  | Two of int * int
  | Four of int * int * int * int

let should_turn_pos newObst lst (x, y) c =
  match newObst with
  | Two (sx, sy) -> (sx = x && sy = y) || (should_turn lst (x, y) c)
  | _ -> false
;;

let comp t1 t2 =
  match (t1, t2) with
  | (Two (a1, b1), Two (a2, b2)) -> a1 = a2 && b1 = b2
  | (Four (a1, b1, c1, d1), Four (a2, b2, c2, d2)) -> a1 = a2 && b1 = b2 && c1 = c2 && d1 = d2
  | _ -> false (* Two-element tuple cannot be equal to a four-element tuple *)


let rec add_uniq lst (a, b, c, d) =
  match lst with
  | [] -> [(a, b, c, d)]
  | (x, y, z, w) :: rest -> if a = x && b = y then ((x, y, z, w) :: rest) else ((x, y, z, w) :: add_uniq rest (a, b, c, d))
;;

let rec has lst t =
  match lst with
  | [] -> false
  | hd::rest -> if comp t hd then true else has rest t
;;

exception Circle;;

let rec go sht met keep (dx, dy) (x, y) lst =
  if is_end lst (x + dx, y + dy) then
    add_uniq keep (x, y, dx, dy)
  else if sht lst (x + dx, y + dy) '#' then
    if has met (Four (x, y, dx, dy)) then
      raise Circle
    else
      go sht (Four (x, y, dx, dy) :: met) keep (turn (dx, dy)) (x, y) lst
  else
    add_uniq (go sht met keep (dx, dy) (x + dx, y + dy) lst) (x, y, dx, dy)
;;

let build content start =
  let path = go (should_turn) [] [] (0, -1) start content in
  let rec trav p = 
    match p with
    | [] -> []
    | (x, y, dx, dy)::rest -> 
        try let _ = go (should_turn_pos (Two(x, y))) [] [] (0, -1) start content in 
          (None :: (trav rest))
        with 
        | Circle -> (Some ((x, y, dx, dy)) :: (trav rest))
    in
  List.filter_map (fun e -> e) (trav path)
;;

let prep f = 
  In_channel.with_open_bin f In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
;; 

let content = prep file;;
let find_symb = find content;;

let result () = List.length (build content (find_symb '^'));; 

