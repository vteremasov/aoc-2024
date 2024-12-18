let file = "./day_18/input/input";;

type point = {
  x : int;
  y : int;
};;

let get_dirs = 
  [
    {x = -1; y = 0};
    {x = 0; y = -1};
    {x = 1; y = 0 };
    {x = 0; y = 1 };
  ]
;;

let add p1 p2 =
  {x = p1.x + p2.x; y = p1.y + p2.y}
;;

let can_go map2d p =
  let max_y = Array.length map2d - 1 in
  let max_x = Array.length map2d.(0) -1 in
  if p.x > max_x || p.y > max_y || p.x < 0 || p.y < 0 then false
  else map2d.(p.y).(p.x) <> '#'
;;

let build_map h w pairs =
  (* Apparently I need to tuch each element 
     Otherwise mutating first element of internal array will 
     mutate all it's copies *)
  let map2d = Array.map (fun arr -> Array.map (Fun.id) arr) (Array.make (h + 1) (Array.make (w + 1) '.')) in

  List.iter (fun p -> map2d.(p.y).(p.x) <- '#') pairs;

  map2d
;;

let get_neighbors map2d current =
  let points = List.map (fun d -> add d current) (get_dirs) in
  List.map (fun p -> (1, p)) (List.filter (fun p -> can_go map2d p) points)
;;

let parse_pair str =
  let lst = Lib.splitrim ',' str in
  match lst with
  | x::y::_ -> {x = int_of_string x; y = int_of_string y}
  | _ -> failwith (Printf.sprintf "It's a bad pair %s" str)
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

let dijkstra map2d source =
  let distances = Hashtbl.create (Array.length map2d) in
  let visited = Hashtbl.create (Array.length map2d) in
  let rec loop queue =
    match queue with
    | [] -> ()
    | _ ->
        let (dist, current), rest_queue = priority queue in
        if Hashtbl.mem visited current then loop rest_queue
        else (
          Hashtbl.add visited current true;
          let neighbors = get_neighbors map2d current in
          let new_queue =
            List.fold_left
              (fun acc (weight, neighbor) ->
                let new_dist = dist + weight in
                let (old_dist, _) =
                  if Hashtbl.mem distances neighbor then
                    Hashtbl.find distances neighbor
                  else (max_int, neighbor)
                in
                if new_dist < old_dist then (
                  Hashtbl.replace distances neighbor (new_dist, neighbor);
                  (new_dist, neighbor) :: acc
                ) else acc)
              rest_queue neighbors
          in
          loop new_queue)
  in
  Hashtbl.add distances source (0, source);
  loop [(0, source)];
  distances
;;

let clear_screen () =
  Printf.printf "\027[2J";  (* Clear screen *)
  Printf.printf "\027[H";   (* Move cursor to the top-left *)
  flush stdout
;;

let draw = fun map ->
  clear_screen ();
  Printf.printf "%s%!" (
    String.concat "\n" (
      Array.to_list (
        Array.map (
          fun arr -> String.concat "" (List.map (fun chr -> String.make 1 chr) (Array.to_list arr))
          ) map
        )
      )
  );
  flush stdout;
  Unix.sleepf 0.1;  (* Delay for 0.1 seconds *)
;;

let get_path f =
  let width  = 70                      in
  let height = 70                      in
  let bts    = 1024                    in
  let start  = {x = 0; y = 0}          in
  let exit   = {x = width; y = height} in
  In_channel.with_open_bin f In_channel.input_all
  |> Lib.splitrim '\n'
  |> List.map (parse_pair)
  |> List.to_seq
  |> fun pairs -> build_map width height (List.of_seq (Seq.take bts pairs))
  |> fun map2d -> dijkstra map2d start
  |> fun all -> fst(Hashtbl.find all exit)
;;


let result () = get_path file;;

