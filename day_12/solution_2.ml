let file = "./day_12/input/example";;

let prepare f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.trim
  |> String.split_on_char '\n'
;;

let dimensions grid =
  let rows = List.length grid in
  let cols = if rows = 0 then 0 else String.length (List.hd grid) in
  (rows, cols)
;;

let char_at grid (x, y) =
  let rows, cols = dimensions grid in
  if x >= 0 && x < rows && y >= 0 && y < cols then
    Some (String.get (List.nth grid x) y)
  else
    None
;;

let neighbors_dir (x, y) =
  [(x-1, y); (x, y-1); (x+1, y); (x, y+1)]
;;

let neighbors_dag (x, y) =
  [(x-1, y-1); (x+1, y-1); (x-1, y+1); (x+1, y+1)]
;;


let fd_fill grid start visited =
  let rec aux queue component =
    match queue with
    | [] -> component
    | (x, y) :: rest ->
      if Hashtbl.mem visited (x, y) then
        aux rest component
      else
        match char_at grid (x, y) with
        | None -> aux rest component
        | Some current_chr ->
          Hashtbl.add visited (x, y) true;
          let valid_neighbors =
            List.filter (fun coord ->
              match char_at grid coord with
              | Some c when c = current_chr && not (Hashtbl.mem visited coord) -> true
              | _ -> false)
            (neighbors_dir (x, y))
          in
          aux (valid_neighbors @ rest) ((x, y) :: component)
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

let count_corners coords =
  let res = ref 0 in
  List.iter (fun (x, y) -> 
    let nbs = neighbors_dag (x, y) in
    List.iter (fun (nx, ny) ->
      let nxin = List.mem (nx, y) coords in
      let nyin = List.mem (x, ny) coords in
      if not nyin && not nxin then
        incr res;
      if nyin && nxin && not (List.mem (nx, ny) coords) then
        incr res;
    ) nbs
  ) coords;

  !res
;;

let content = prepare file;;

let result () = group_by content |> List.fold_left (fun acc coords -> acc + ((List.length coords) * (count_corners coords))) 0;

