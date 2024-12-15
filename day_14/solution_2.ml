let file = "./day_14/input/input";;

type point = {
  x : int;
  y : int;
}

type robot = {
  pos : point;
  vel : point;
}

let height = 103;;
let width  = 101;;
let time   = 1;;

let get_robot str =
  let regex = Str.regexp "[-1]?[0-9]+" in (* Example regex *)
  let rec collect_matches pos acc =
    if Str.string_match regex str pos then
      let num = int_of_string (Str.matched_string str) in
      collect_matches (Str.match_end ()) (num :: acc)
    else if (String.length str) > pos then (
      collect_matches (pos + 1) acc)
    else (
      List.rev acc)
  in
  let numbers = collect_matches 0 [] in
  match numbers with
  | [p1; p2; v1; v2] -> 
      { pos = { x = p1; y = p2 }; vel = { x = v1; y = v2 } }
  | _ -> 
      failwith "Unexpected"

let add pos vel time lh lw = 
  let new_x = (pos.x + (vel.x * time)) mod lw in
  let new_y = (pos.y + (vel.y * time)) mod lh in
  {
    x = if new_x >= 0 then new_x else lw + new_x;
    y = if new_y >= 0 then new_y else lh + new_y;
  }
;;


let move time r =
  let pos = add r.pos r.vel time height width in
  {pos = pos; vel = r.vel;}
;;

let prepare f =
  In_channel.with_open_bin f In_channel.input_all
  |> Lib.splitrim '\n'
  |> List.map (fun str -> get_robot str)
;;

let to_map h w robots =
  let cache = List.fold_left (fun acc robot ->
    Hashtbl.add acc ((string_of_int robot.pos.x)^"|"^(string_of_int robot.pos.y)) true;
    acc
  ) (Hashtbl.create 1024) robots in
  let str = ref "" in
  for y = 0 to (h - 1) do
    for x = 0 to (w - 1) do
      if Hashtbl.mem cache ((string_of_int x)^"|"^(string_of_int y)) then 
        str := !str ^ "#"
      else
        str := !str ^ "."
    done;
    str := !str ^ "\n"
  done;

  !str
;;

let unwrap op =
  match op with
  | Some n -> n
  | None -> failwith "Oh nooo"
;;

let step robots =
  robots := List.map (fun robot -> move 1 robot) !robots;
  !robots
;;

let find pats str =
  let pos = ref 0 in
  try
    List.iter (fun pat -> 
      pos := Str.search_forward pat str !pos
    ) pats;
  Printf.printf "%s" str;
  true
  with Not_found -> false
;;

let get_tree () =
  let tree = [
      Str.regexp_string "....#....";
      Str.regexp_string "...###...";
      Str.regexp_string "..#####..";
      Str.regexp_string ".#######.";
    ] in
  let robots = ref (prepare file) in
  Seq.forever (fun () -> step robots |> to_map height width)
      |> Seq.find_index (find tree)
      |> unwrap
;;

let result () = get_tree () + 1;;

