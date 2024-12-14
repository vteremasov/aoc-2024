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
let time   = 100;;

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
  |> List.map (move time)
;;

let get_quadrants robots w h =
  let lt = ref [] in
  let rt = ref [] in
  let lb = ref [] in
  let rb = ref [] in
  let rec aux rbs =
    match rbs with
    | [] -> ()
    | hd::rest when hd.pos.x = (w / 2) || hd.pos.y = (h / 2) -> aux rest
    | hd::rest when hd.pos.x < (w / 2) && hd.pos.y < (h / 2) -> lt := hd::!lt; aux rest
    | hd::rest when hd.pos.x > (w / 2) && hd.pos.y < (h / 2) -> rt := hd::!rt; aux rest
    | hd::rest when hd.pos.x < (w / 2) && hd.pos.y > (h / 2) -> lb := hd::!lb; aux rest
    | hd::rest when hd.pos.x > (w / 2) && hd.pos.y > (h / 2) -> rb := hd::!rb; aux rest
    | _ -> ()
  in
    aux robots;
    List.length !lt * List.length !rt * List.length !lb * List.length !rb
;;

let result () = get_quadrants (prepare file) width height;;

