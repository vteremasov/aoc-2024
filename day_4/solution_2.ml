let file = "./day_4/input/input2.txt";;

let file_content f = In_channel.with_open_bin f In_channel.input_all;;

let prepare str =
  String.split_on_char '\n' str 
  |> List.map (fun s -> String.to_seq s |> List.of_seq);;

let get_char lst (x, y) =
  let row = try List.nth lst (y) with
  | Invalid_argument _ -> [];
  in
  try Some(List.nth row (x)) with
  | Failure _ -> None
  | Invalid_argument _ -> None

let check lst (x, y) =
  let tl = get_char lst (x - 1, y - 1)
  in
  let tr = get_char lst (x + 1, y - 1)
  in
  let bl = get_char lst (x - 1, y + 1)
  in
  let br = get_char lst (x + 1, y + 1)
  in
  match (tl, tr, bl, br) with
  | (Some('M'), Some('M'), Some('S'), Some('S')) -> 1
  | (Some('S'), Some('S'), Some('M'), Some('M')) -> 1
  | (Some('S'), Some('M'), Some('S'), Some('M')) -> 1
  | (Some('M'), Some('S'), Some('M'), Some('S')) -> 1
  | _ -> 0;;


let result () = let grid = prepare (file_content file) in
              grid
              |> List.mapi (fun y row -> 
                    List.mapi (fun x ch -> if ch = 'A' then check grid (x, y) else 0) row
                  )
              |> List.fold_left (fun acc arr -> acc @ arr) []
              |> List.fold_left  (+) 0
;;

