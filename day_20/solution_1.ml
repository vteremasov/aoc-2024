let file = "./day_20/input/example";;

type point = {
  mutable x : int;
  mutable y : int;
};;

type tile = {
  pos : point;
  mutable chr : char;
}

type map2d_t = tile array array;;

let clear_screen () =
  Printf.printf "\027[2J";  (* Clear screen *)
  Printf.printf "\027[H";   (* Move cursor to the top-left *)
  flush stdout
;;

let draw = fun map hash ->
  (* clear_screen (); *)
  Printf.printf "%s%!" (
    String.concat "\n" (
      Array.to_list (
        Array.map (
          fun arr -> String.concat "" (List.map (fun t -> 
            if Hashtbl.fold (fun (_, tile) _ acc -> if acc then acc else (tile.pos = t.pos)) hash false then
              String.make 1 'o'
            else 
              String.make 1 t.chr) (Array.to_list arr))
          ) map
        )
      )
  );
  flush stdout;
  Unix.sleepf 0.1;  (* Delay for 0.1 seconds *)
;;

let add p1 p2 =
  {x = p1.x + p2.x; y = p1.y + p2.y}
;;

let get_dirs = 
  [
    {x = -1; y = 0};
    {x = 0; y = -1};
    {x = 1; y = 0 };
    {x = 0; y = 1 };
  ]
;;

let can_go map2d p =
  let max_y = Array.length map2d - 1 in
  let max_x = Array.length map2d.(0) -1 in
  if p.x > max_x || p.y > max_y || p.x < 0 || p.y < 0 then false
  else map2d.(p.y).(p.x).chr <> '#'
;;

let can_remove_wall map2d p =
  let max_y = Array.length map2d - 1 in
  let max_x = Array.length map2d.(0) -1 in
  if p.x > max_x || p.y > max_y || p.x < 0 || p.y < 0 then false
  else map2d.(p.y).(p.x).chr = '#'
;;

type get_neighbors_t = map2d_t -> (int * int) * tile -> (int * ((int * int) * tile)) list;;
let get_neighbors : get_neighbors_t = fun map2d (cheat, current) ->
  let points = List.map (fun d -> (cheat, add d current.pos)) (get_dirs) in
  let neighbors = List.map (fun (c, p) -> (1, (c, map2d.(p.y).(p.x)))) (List.filter (fun (_, p) -> can_go map2d p) points) in
  neighbors
;;

let rec priority = fun queue ->
  match queue with
  | [] -> failwith "Empty queue"
  | [x] -> x, []
  | x :: xs ->
      let min_rest, rest_queue = priority xs in
      if fst x <= fst min_rest then x, xs
      else min_rest, x :: rest_queue
;;

type distances_t = ((int * int) * tile, int * tile * tile) Hashtbl.t;;
type dijkstra_t = map2d_t -> tile -> distances_t;;
let dijkstra : dijkstra_t = fun map2d source ->
  let distances = Hashtbl.create (Array.length map2d) in
  let visited = Hashtbl.create (Array.length map2d) in
  let rec loop queue =
    (* draw map2d distances; *)
    match queue with
    | [] -> ()
    | _ ->
        let (dist, (cheat, current)), rest_queue = priority queue in
        if Hashtbl.mem visited (cheat, current) then loop rest_queue
        else (
          Hashtbl.add visited (cheat, current) true;
          let neighbors = get_neighbors map2d (cheat, current) in
          let new_queue =
            List.fold_left
              (fun acc (weight, (n_cheat, neighbor)) ->
                let new_dist = dist + weight in
                let (old_dist, _, prev) =
                  if Hashtbl.mem distances (n_cheat, neighbor) then
                    Hashtbl.find distances (n_cheat, neighbor)
                  else (max_int, neighbor, current)
                in
                if new_dist < old_dist then (
                  Hashtbl.replace distances (n_cheat, neighbor) (new_dist, neighbor, prev);
                  (new_dist, (n_cheat, neighbor)) :: acc
                ) else acc)
              rest_queue neighbors
          in
          loop new_queue)
  in
  Hashtbl.add distances ((0, 0), source) (0, source, source);
  loop [(0, ((0, 0), source))];
  distances
;;


let to_map lst =
  let m = Array.of_list (List.map (fun str -> Array.of_seq (String.to_seq str)) lst) in
  Array.mapi (fun y row -> Array.mapi (fun x chr -> {pos = {x; y;}; chr;}) row) m
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

let fst3 (a, _, _) = a;;
let thd3 (_, _, c) = c;;

type get_path_t = distances_t -> tile -> int * tile * tile -> tile list
let get_path : get_path_t = fun hash start en ->
  let res = ref ([thd3 en]) in
  let prevs = ref (thd3 en) in
  while !prevs <> start do
    prevs := thd3 (Hashtbl.find hash ((0,0), !prevs));
    res := !prevs::!res;
  done;

  !res
;;

let surr_point : map2d_t -> point -> point list = fun map2d point ->
  let points = List.map (fun p -> add point p) get_dirs in
  List.filter (fun p -> can_remove_wall map2d p) points
;;

type get_walls_t = map2d_t -> tile list -> point list;;
let rec get_walls : get_walls_t = fun map2d lst ->
  match lst with
  | [] -> []
  | hd::rest -> (surr_point map2d hd.pos) @ (get_walls map2d rest)
;;

let get_new_map map2d p =
  Array.map (fun row -> Array.map (fun t -> if t.pos = p then {pos = t.pos; chr = '.'} else t) row) map2d
;;

let uniq lst =
  List.fold_left (fun acc e -> if List.mem e acc then acc else e :: acc) [] lst
;;

let get_all_dists map2d start fin walls = 
  List.map (fun wall ->
    let new_map = get_new_map map2d wall in
    let dist = dijkstra new_map start in
    fst3 (Hashtbl.find dist ((0, 0), fin))
  ) walls
;;

let get_paths f =
  In_channel.with_open_bin f In_channel.input_all
  |> Lib.splitrim '\n'
  |> fun lst ->
    let map2d = to_map lst in
    let start = get_tile map2d 'S' in
    let fin = get_tile map2d 'E' in
    let dis = dijkstra map2d start in
    let simp_end = Hashtbl.find dis ((0, 0), fin) in
    let walls = uniq (get_walls map2d (get_path dis start simp_end)) in
    let dists = get_all_dists map2d start fin walls in
    List.length (List.filter (fun d -> fst3(simp_end) - d >= 100) dists)
;;

(* let result () = get_paths file;; *)
let result () = 0;;

