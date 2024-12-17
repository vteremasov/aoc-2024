let file = "./day_16/input/input";;

type point = {
  x : int;
  y : int;
}

type tile = {
  mutable chr : char;
  mutable pos : point;
  is_robot    : bool;
};;

type grid = tile array array;;

let clear_screen () =
  Printf.printf "\027[2J";  (* Clear screen *)
  Printf.printf "\027[H";   (* Move cursor to the top-left *)
  flush stdout
;;

let snd3 (_, b, _) = b;;

let draw = fun map hash ->
  (* clear_screen (); *)
  Printf.printf "%s%!" (
    String.concat "\n" (
      Array.to_list (
        Array.map (
          fun arr -> String.concat "" (List.map (fun t -> 
            if List.mem t hash then
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

let to_dir a =
  match a with
  | '<' -> {x = -1; y =  0}
  | '^' -> {x =  0; y = -1}
  | '>' -> {x =  1; y =  0}
  | 'v' -> {x =  0; y =  1}
  | c -> failwith (Printf.sprintf "Incorrect direction '%c'" c)
;;

let add p1 p2 =
  {x = p1.x + p2.x; y = p1.y + p2.y;}
;;

let get_tile grid chr is_robot =
  let pnt = ref {x = -1; y = -1} in
  for y = 0 to (Array.length grid - 1) do
    for x = 0 to (Array.length grid.(y) - 1) do
      if grid.(y).(x).chr = chr then
        pnt := {x;y}
    done
  done;

  {chr; pos = !pnt; is_robot;}
;;

let of_map str =
  Array.of_list (
    List.mapi (fun y str -> 
      Array.of_seq (
        String.to_seq str |> Seq.mapi (fun x chr -> {pos = {x;y}; chr; is_robot = false;}))
      ) (Lib.splitrim '\n' str)
  )
;;

let fst3: 'a * 'b * 'c -> 'a = fun (a, _, _) -> a;;

type priority_t = (int * char * tile) list -> (int * char * tile) * (int * char * tile) list;;
let rec priority : priority_t = fun queue ->
  match queue with
  | [] -> failwith "Empty queue"
  | [x] -> x, []
  | x :: xs ->
      let min_rest, rest_queue = priority xs in
      if fst3 x <= fst3 min_rest then x, xs
      else min_rest, x :: rest_queue
;;

let rec get_min = fun lst ->
  match lst with
  | [] -> failwith "Empty queue"
  | [x] -> x, []
  | x :: xs ->
      let min_rest, rest_queue = get_min xs in
      if fst3 x <= fst3 min_rest then x, xs
      else min_rest, x :: rest_queue
;;

let get_other_dirs dir = 
  match dir with
  | '<' -> ['^';'>';'v']
  | '^' -> ['>';'v';'<']
  | '>' -> ['v';'<';'^']
  | 'v' -> ['<';'^';'>']
  | c   -> failwith (Printf.sprintf "Incorrect dir %c" c)
;;

let is_wall point map =
  map.(point.y).(point.x).chr = '#'
;;

type get_neighbors_t = tile array array -> point -> char -> (int * char * tile) list;;
let get_neighbors : get_neighbors_t = fun map curr_pos curr_dir ->
  let moves = (1, curr_dir, to_dir curr_dir)::(List.map (fun d -> (1001, d, to_dir d)) (get_other_dirs curr_dir)) in
  let new_pos = List.filter (fun (_, _, p) -> not (is_wall p map)) (List.map(fun (weight, dir, move) -> (weight, dir, add curr_pos move)) moves) in
  List.map (fun (w, d, p) -> (w, d, map.(p.y).(p.x))) new_pos
;;

let dijkstra map2d source direction =
  let distances = Hashtbl.create (Array.length map2d) in
  let visited = Hashtbl.create (Array.length map2d) in
  let rec loop queue =
    match queue with
    | [] -> ()
    | _ ->
        let (curr_dist, curr_dir, curr_tile), rest_queue = priority queue in
        let neighbors = get_neighbors map2d curr_tile.pos curr_dir in
        let new_queue = List.fold_left (fun acc (weight, n_dir, n_tile) ->
          let new_dist = curr_dist + weight in
          if Hashtbl.mem visited (n_tile, n_dir) then
            let (old_dist, dir, preds) = Hashtbl.find distances (n_tile, n_dir) in
            if new_dist < old_dist then (
              Hashtbl.replace distances (n_tile, n_dir) (new_dist, dir, [(curr_dist, curr_dir, curr_tile)]);
              (new_dist, curr_dir, curr_tile)::acc
            )
            else if new_dist = old_dist then (
              Hashtbl.replace distances (n_tile, n_dir) (old_dist, dir, (curr_dist, curr_dir, curr_tile)::preds);
              acc
            ) else (
              acc
            )
          else (
              Hashtbl.add visited (n_tile, n_dir) true;
              Hashtbl.add distances (n_tile, n_dir) (new_dist, n_dir, [(curr_dist, curr_dir, curr_tile)]);
              (new_dist, n_dir, n_tile)::acc
            )
        ) rest_queue neighbors in 
        loop new_queue
  in
  Hashtbl.add distances (source, direction) (0, direction, []);
  Hashtbl.add visited (source, direction) true;
  loop [(0, direction, source)];
  distances
;;

let thd3 (_, _, c) = c;;

let rec add_uniq lst tile =
  match lst with
  | [] -> [tile]
  | hd::rest -> if hd.pos.x = tile.pos.x && hd.pos.y = tile.pos.y then hd::rest else hd::add_uniq rest tile
;;

let count_path : tile array array -> ((tile * char), (int * char * (int * char * tile) list)) Hashtbl.t -> int = fun map hash ->
  let ends = Hashtbl.fold (fun (tile, _) v acc -> if tile.chr = 'E' then v::acc else acc) hash [] in
  let result = ref [get_tile map 'E' false] in
  let min, _ = get_min ends in
  let rec loop preds =
    match preds with
    | [] -> ()
    | _ -> List.iter (fun (_, dir, tile) ->
          result := add_uniq !result tile;
          let v = Hashtbl.find hash (tile, dir) in
          loop (thd3 v)
        ) preds
  in
  loop (thd3 min);
  List.length !result
;;

let get_places f =
  In_channel.with_open_bin f In_channel.input_all
  |> of_map
  |> fun map -> dijkstra map (get_tile map 'S' true) '>'
  |> count_path map
;;

let result () = get_places file;;

