let file = "./day_8/input/input";;

let vec_sub (a, b) (a1, b1) =
  (a - a1, b - b1)
;;
let vec_add (a, b) (a1, b1) =
  (a + a1, b + b1)
;;

let collect_pos map =
  let coll_row y row =
    let rec aux x acc = function
      | [] -> List.rev acc
      | c :: cs when c <> '.' -> aux (x + 1) ((c, x, y) :: acc) cs
      | _ :: cs -> aux (x + 1) acc cs
    in
    aux 0 [] (List.of_seq (String.to_seq row))
  in
  let rec collect y acc = function
    | [] -> List.rev acc
    | row :: rows -> collect (y + 1) (coll_row y row @ acc) rows
  in
  collect 0 [] map
;;

let group_by positions =
  positions
  |> List.fold_left (fun acc (c, x, y) ->
    let existing = try List.assoc c acc with Not_found -> [] in
    (c, (x, y) :: existing) :: List.remove_assoc c acc
  ) []
  |> List.map (fun (c, coords) -> (c, List.rev coords))
;;

let rec permutate lst =
  match lst with
  | [] -> []
  | a :: rest -> List.map (fun b -> (a, b, vec_sub a b)) rest @ permutate rest
;;

let permutate_freq lst = List.map (fun (c, l) -> (c, permutate l)) lst;;

let contains map (x, y) =
  x >= 0 && y >= 0 && y < List.length map && x < String.length (List.hd map)
;;

let is_untinode map c (x, y) =
  contains map (x, y) && c != String.get (List.nth map y) x
;;

let uniq lst elem =
  if List.mem elem lst then lst else elem::lst
;;

let get_count f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
  |> (fun map -> collect_pos map |> group_by |> permutate_freq |> fun coords -> (map, coords))
  |> fun (map, coords) -> 
      List.fold_left (fun acc (c, lst) -> 
        acc @ List.map (fun (a, b, d) -> 
          [
            (is_untinode map c (vec_add a d), vec_add a d); 
            (is_untinode map c (vec_sub b d), vec_sub b d)
          ]) lst) [] coords
  |> List.flatten
  |> List.filter (fun (b, _) -> b)
  |> List.fold_left uniq []
  |> List.length
;;

let result () = get_count file;;

