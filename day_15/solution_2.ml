let file = "./day_15/input/input";;

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

let draw : tile array array -> unit = fun map ->
  Printf.printf "%s%!" (
    String.concat "\n" (
      Array.to_list (
        Array.map (
          fun arr -> String.concat "" (List.map (fun t -> String.make 1 t.chr) (Array.to_list arr))
          ) map
        )
      )
  );
  flush stdout;
  Unix.sleepf 0.01;  (* Delay for 0.1 seconds *)
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

let get_robot grid =
  let pnt = ref {x = -1; y = -1} in
  for y = 0 to (Array.length grid - 1) do
    for x = 0 to (Array.length grid.(y) - 1) do
      if grid.(y).(x).chr = '@' then
        pnt := {x;y}
    done
  done;

  {chr = '@'; pos = !pnt; is_robot = true;}
;;

let is_box : tile -> bool = fun tile ->
  tile.chr = '[' || tile.chr = ']'
;;

let get_box : tile array array -> tile -> tile * tile = fun map tile ->
  match tile.chr with
  | '[' -> let n = add tile.pos (to_dir '>') in (tile, map.(n.y).(n.x))
  | ']' -> let n = add tile.pos (to_dir '<') in (map.(n.y).(n.x), tile)
  | c -> failwith (Printf.sprintf "Oh no, it's not a box %c" c)
;;

let of_map str =
  Array.of_list (
    List.mapi (fun y str -> 
      Array.of_seq (
        String.to_seq str
        |> Seq.flat_map (fun chr -> 
            match chr with
            | '#' -> String.to_seq "##"
            | '.' -> String.to_seq ".."
            | 'O' -> String.to_seq "[]"
            | '@' -> String.to_seq (Printf.sprintf "%c." '@')
            | c   -> failwith (Printf.sprintf "Oh no, it's incorrect tile %c" c)
        )
        |> Seq.mapi (fun x chr -> {pos = {x;y}; chr; is_robot = false;}))
      ) (Lib.splitrim '\n' str)
  )
;;

type bfs_t = tile array array -> tile * tile -> char -> tile list;;
let bfs : bfs_t = fun map (t1, t2) ptr ->
  let visited = Hashtbl.create 100 in
  let queue = Queue.create () in
  let result = ref [] in
  Queue.add t1 queue;
  Queue.add t2 queue;
  let rec loop () =
    if not (Queue.is_empty queue) then (
      let current = Queue.take queue in
      if not (Hashtbl.mem visited current) then (
        Hashtbl.add visited current true;
        result := current::!result;

        let next = add current.pos (to_dir ptr) in

        if is_box map.(next.y).(next.x) then (
          let (tt1, ll1) = get_box map map.(next.y).(next.x) in
          Queue.add tt1 queue;
          Queue.add ll1 queue
        )
      );
      loop ()
    )
  in
  loop ();

  !result
;;

type make_move_t = tile array array -> tile -> char -> bool;;
let make_move : make_move_t = fun map tl ptr ->
  let dir = to_dir ptr in
  let next = add tl.pos dir in
  let replace tt pp =
    map.(pp.y).(pp.x).chr <- tt.chr;
    map.(tt.pos.y).(tt.pos.x).chr <- '.';
    if tt.is_robot then tt.pos <- pp;
  in

  if map.(next.y).(next.x).chr = '#' then (
    false
  )

  else if is_box map.(next.y).(next.x) then (
    let boxes = bfs map (get_box map map.(next.y).(next.x)) ptr in
    let move_posible = List.for_all (
        fun tt -> let nn = add tt.pos (to_dir ptr) in map.(nn.y).(nn.x).chr != '#'
      ) boxes in
    if not move_posible then false
    else (
      let sorted = List.sort (
        fun a b -> 
          let diff_a = abs (a.pos.x - next.x) + abs (a.pos.y - next.y) in
          let diff_b = abs (b.pos.x - next.x) + abs (b.pos.y - next.y) in
          if diff_b > diff_a then 1
          else if diff_b < diff_a then -1
          else 0
      ) boxes in
      List.iter (fun t -> replace t (add t.pos (to_dir ptr))) sorted;
      replace tl next;
      true
    )
  )
  else if map.(next.y).(next.x).chr = '.' then (
    replace tl next;
    true
  )
  else (
    false
  )
;;

type make_moves_t = bool -> tile * char list * tile array array -> tile array array;;
let  make_moves : make_moves_t = fun should_draw (robot, ptrs, map) ->
  List.iter (fun ptr ->
    let moved = make_move map robot ptr in
    if moved && should_draw then (
      clear_screen ();
      draw map;
    )
  ) ptrs;
  map
;;

let prepare f animate =
  In_channel.with_open_bin f In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> (fun lst -> 
      match lst with 
      | map::moves::_ -> (map, moves) 
      | _ -> failwith "Oh no")
  |> fun (grid, moves) -> (of_map grid, String.to_seq (String.concat "" (Lib.splitrim '\n' moves)))
  |> fun (map, moves) -> (get_robot map, List.of_seq moves, map)
  |> make_moves animate
  |> Array.fold_left Array.append [||]
  |> Array.to_list
  |> List.filter (fun tile -> tile.chr = '[')
  |> List.fold_left (fun acc tile -> acc + (tile.pos.x + (tile.pos.y * 100))) 0
;;

let result () = prepare file false;

