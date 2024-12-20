let file = "./day_20/input/input"

type point = {
  mutable x : int;
  mutable y : int;
};;

type tile = {
  pos : point;
  mutable chr : char;
}

type map2d_t = tile array array;;

let to_map lst =
  let m = Array.of_list (List.map (fun str -> Array.of_seq (String.to_seq str)) lst) in
  Array.mapi (fun y row -> Array.mapi (fun x chr -> {pos = {x; y;}; chr;}) row) m
;;

let map2d = In_channel.with_open_bin file In_channel.input_all |> Lib.splitrim '\n' |> to_map

let within_20_distance =
  let lst = ref [] in
  for y = -20 to 20 do
    for x = -20 to 20 do
      if abs x + abs y <= 20 && not (x = 0 && y = 0) then lst := {x;y;} :: !lst
    done
  done;
  !lst
;;

let diff a b = abs (a.x - b.x) + abs (a.y - b.y);;
let add p1 p2 = {x = p1.x + p2.x; y = p1.y + p2.y};;

let get_dirs = 
  [
    {x = -1; y = 0};
    {x = 0; y = -1};
    {x = 1; y = 0 };
    {x = 0; y = 1 };
  ]
;;

let get_tile map2d chr =
  let tile = {pos = {x = -1; y = -1}; chr} in

  for y = 0 to (Array.length map2d) - 1 do
    for x = 0 to (Array.length map2d.(0)) - 1 do
      if tile.chr = map2d.(y).(x).chr then (
        tile.pos.x <- x;
        tile.pos.y <- y;
      )
    done
  done;

  tile
;;

let create = Array.make;;
let score_arr =
  Array.map (fun row -> Array.map (Fun.id) row) (Array.make (Array.length map2d) (Array.make (Array.length map2d.(0)) (-1)))
;;

let can_go map2d p =
  let max_y = Array.length map2d - 1 in
  let max_x = Array.length map2d.(0) -1 in
  if p.x > max_x || p.y > max_y || p.x < 0 || p.y < 0 then false
  else map2d.(p.y).(p.x).chr <> '#'
;;

let bounds map2d p =
  let max_y = Array.length map2d - 1 in
  let max_x = Array.length map2d.(0) -1 in
  if p.x > max_x || p.y > max_y || p.x < 0 || p.y < 0 then false
  else true
;;


type get_neighbors_t = map2d_t -> tile -> tile list;;
let get_neighbors : get_neighbors_t = fun map2d current ->
  let points = List.map (fun d -> add d current.pos) (get_dirs) in
  let neighbors = List.map (fun p -> map2d.(p.y).(p.x)) (List.filter (fun p -> can_go map2d p) points) in
  neighbors
;;

let start_pos = get_tile map2d 'S'
let end_pos = get_tile map2d 'E'

let next_pos prev curr =
  get_neighbors map2d curr
  |> List.find (fun n_tile -> not (n_tile.pos = prev.pos))
;;

let rec walk_path prev curr =
  if curr.pos = end_pos.pos then (
    score_arr.(end_pos.pos.y).(end_pos.pos.x) <- 0;
    0)
  else (
    let next_pos = next_pos prev curr in
    let score = walk_path curr next_pos in
    score_arr.(curr.pos.y).(curr.pos.x) <- score + 1;
    score + 1)
;;

let _ = walk_path map2d.(0).(0) start_pos;;

let count_valid_cheats cheat_fn =
  let count = ref 0 in

  let rec walk prev curr =
    if curr.pos = end_pos.pos then ()
    else (
      count := !count + cheat_fn curr;
      let next_pos = next_pos prev curr in
      walk curr next_pos)
  in

  walk map2d.(0).(0) start_pos;

  !count
;;

let part2 min_saves =
  let cheat_fn curr =
    within_20_distance
    |> List.map (fun v -> add curr.pos v)
    |> List.filter (fun p -> bounds map2d p)
    |> List.filter_map (fun p ->
           match score_arr.(p.y).(p.x) with
           | -1 -> None
           | s -> Some (s + diff p curr.pos))
    |> List.filter (fun s ->
           (score_arr.(curr.pos.y).(curr.pos.x)) - s >= min_saves)
    |> List.length
  in

  count_valid_cheats cheat_fn
;;

let result () = part2 100;;

