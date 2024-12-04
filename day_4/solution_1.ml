let file = "./day_4/input/input1.txt";;

let file_content f = In_channel.with_open_bin f In_channel.input_all;;

let prepare str =
  String.split_on_char '\n' str 
  |> List.map (fun s -> String.to_seq s |> List.of_seq);;

let rec count_in lst (dx, dy) p (x, y) =
  let row = try List.nth lst (y + dy) with
  | Invalid_argument _ -> [];
  in
  let ch = try List.nth row (x + dx) with
  | Failure _ -> '$'
  | Invalid_argument _ -> '$';
  in
  match (p, ch) with
  | (_, '$') -> 0 
  | ('^', 'X') -> count_in lst (-1,  0) 'X' (x, y) +
                  count_in lst (1,   0) 'X' (x, y) +
                  count_in lst (-1, -1) 'X' (x, y) +
                  count_in lst (1,  -1) 'X' (x, y) +
                  count_in lst (0,   1) 'X' (x, y) +
                  count_in lst (0,  -1) 'X' (x, y) +
                  count_in lst (-1,  1) 'X' (x, y) +
                  count_in lst (1,   1) 'X' (x, y)
  | ('X', 'M') -> count_in lst (dx, dy) 'M' (x + dx, y + dy)
  | ('M', 'A') -> count_in lst (dx, dy) 'A' (x + dx, y + dy)
  | ('A', 'S') -> 1
  | _ -> 0;;

let result () = let grid = prepare (file_content file) in
              grid
              |> List.mapi (fun y row -> 
                    List.mapi (fun x ch -> if ch = 'X' then count_in grid (0, 0) '^' (x, y) else 0) row
                  )
              |> List.fold_left (fun acc arr -> acc @ arr) []
              |> List.fold_left  (+) 0
;;

