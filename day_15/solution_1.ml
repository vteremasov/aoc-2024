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

let anim : tile array array -> unit = fun map ->
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
  Unix.sleepf 0.01;
  clear_screen ();
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
  tile.chr = 'O'
;;

let of_map str =
  Array.of_list (
    List.mapi (fun y str -> 
      Array.of_seq (
        String.to_seq str |> Seq.mapi (fun x chr -> {pos = {x;y}; chr; is_robot = false;}))
      ) (Lib.splitrim '\n' str)
  )
;;

let rec make_move : tile array array -> tile -> char -> bool = fun map tl ptr ->
  let dir = to_dir ptr in
  let next = add tl.pos dir in
  let replace () =
    map.(next.y).(next.x).chr <- tl.chr;
    map.(tl.pos.y).(tl.pos.x).chr <- '.';
    if tl.is_robot then tl.pos <- next;
  in
  if map.(next.y).(next.x).chr = '#' then false
  else if map.(next.y).(next.x).chr = '.' then (
    replace ();
    true
  )
  else if make_move map map.(next.y).(next.x) ptr then (
    replace ();
    true
  )
  else false
;;

let make_moves : bool -> tile * char list * tile array array -> tile array array = fun sa (robot, ptrs, map) ->
  List.iter (fun ptr ->
    let moved = make_move map robot ptr in
    if moved && sa then (
      clear_screen ();
      anim map;
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
  |> fun (map, moves) -> (of_map map, String.to_seq (String.concat "" (Lib.splitrim '\n' moves)))
  |> fun (map, moves) -> (get_robot map, List.of_seq moves, map)
  |> make_moves animate
  |> Array.fold_left Array.append [||]
  |> Array.to_list
  |> List.filter (is_box)
  |> List.fold_left (fun acc tile -> acc + (tile.pos.x + (tile.pos.y * 100))) 0
;;

let result () = prepare file false;

