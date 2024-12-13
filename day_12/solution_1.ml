let file = "./day_12/input/input";;

let prepare f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'
;;

let dimensions grid =
  let rows = List.length grid in
  let cols = if rows = 0 then 0 else String.length (List.hd grid) in
  (rows, cols)

let char_at grid (x, y) =
  let rows, cols = dimensions grid in
  if x >= 0 && x < rows && y >= 0 && y < cols then
    Some (String.get (List.nth grid x) y)
  else
    None

let neighbors (x, y) =
  [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]
;;

let fd_fill grid start visited =
  let rec aux queue segment =
    match queue with
    | [] -> segment
    | (x, y) :: rest ->
      if Hashtbl.mem visited (x, y) then
        aux rest segment
      else
        match char_at grid (x, y) with
        | None -> aux rest segment
        | Some current_chr ->
          Hashtbl.add visited (x, y) true;
          let valid_neighbors =
            List.filter (fun coord ->
              match char_at grid coord with
              | Some c when c = current_chr && not (Hashtbl.mem visited coord) -> true
              | _ -> false)
            (neighbors (x, y))
          in
          aux (valid_neighbors @ rest) ((x, y) :: segment)
  in
  match char_at grid start with
  | None -> []
  | Some _ -> aux [start] []
;;

let group_by grid =
  let visited = Hashtbl.create 100 in
  let rows, cols = dimensions grid in
  let chrs = ref [] in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      if not (Hashtbl.mem visited (x, y)) then
        match char_at grid (x, y) with
        | Some _ ->
          let chr = fd_fill grid (x, y) visited in
          chrs := chr :: !chrs
        | None -> ()
    done
  done;
  !chrs
;;

let unwrap s = 
  match s with
  | Some(c) -> c
  | None -> raise Not_found
;;

let count_fence grid coodrs =
  let t = unwrap (char_at grid (List.hd coodrs)) in
  List.fold_left (fun perimetr coord -> 
    perimetr + (List.fold_left (fun others c ->
      let n = char_at grid c in
      match n with
      | Some chr when chr != t -> others + 1
      | None -> others + 1
      | Some _ -> others
    ) 0 (neighbors coord))
  ) 0 coodrs
;;

let content = prepare file;;
let result () = group_by content |> List.fold_left (fun acc coords -> acc + ((List.length coords) * (count_fence content coords))) 0;

