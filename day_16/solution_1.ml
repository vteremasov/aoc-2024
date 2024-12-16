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

type draw_t = tile array array -> (tile, (int * char)) Hashtbl.t -> unit;;
let draw : draw_t = fun map hash ->
  clear_screen ();
  Printf.printf "%s%!" (
    String.concat "\n" (
      Array.to_list (
        Array.map (
          fun arr -> String.concat "" (List.map (fun t -> 
            if Hashtbl.mem hash t then
              String.make 1 (snd (Hashtbl.find hash t))
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
    (* draw map2d distances; *)
    match queue with
    | [] -> ()
    | _ ->
        let (dist, curr_dir, current), rest_queue = priority queue in
        if Hashtbl.mem visited current then loop rest_queue
        else (
          Hashtbl.add visited current true;
          let neighbors = get_neighbors map2d current.pos curr_dir in
          let new_queue =
            List.fold_left
              (fun acc (weight, dir, neighbor) ->
                let new_dist = dist + weight in
                let (old_dist, _) =
                  if Hashtbl.mem distances neighbor then
                    Hashtbl.find distances neighbor
                  else (max_int, dir)
                in
                if new_dist < old_dist then (
                  Hashtbl.replace distances neighbor (new_dist, dir);
                  (new_dist, dir, neighbor) :: acc
                ) else acc)
              rest_queue neighbors
          in
          loop new_queue)
  in
  Hashtbl.add distances source (0, direction);
  loop [(0, direction, source)];
  distances
;;

let get_short_dist f =
  In_channel.with_open_bin f In_channel.input_all
  |> of_map
  |> fun map -> dijkstra map (get_tile map 'S' true) '>'
  |> fun t -> (fst(Hashtbl.find t (get_tile map 'E' false)))
;;

let result () = get_short_dist file;;

