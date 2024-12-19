open Solution_1;;
let file = "./day_19/input/input";;

let count f =
  let (patterns, designs) = prepare f in
  let max_pat = get_max_len patterns in
  let trie = build_trie patterns in
  let get_count = count_possible trie max_pat in
  List.fold_left (+) 0 (List.map get_count designs)
;;

let result () = count file;;

