let file = "./day_19/input/input";;

let parse_patterns str =
  Lib.splitrim ',' str
;;

let parse_designs str =
  List.filter (fun s -> String.length s > 0) (Lib.splitrim '\n' str)
;;

let parse_all lst =
  match lst with
  | p::d::_ -> (parse_patterns p, parse_designs d)
  | _ -> failwith "Oh no"
;;

let prepare f =
  In_channel.with_open_bin f In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> parse_all
;;

type node = {
  mutable is_end: bool;
  children: (char, node) Hashtbl.t;
};;

type trie = node;;

let create_node () = { is_end = false; children = Hashtbl.create 26 };;

let insert trie word =
  let len = String.length word in
  let rec aux node i =
    if i = len then
      node.is_end <- true
    else
      let c = word.[i] in
      let child =
        if Hashtbl.mem node.children c then
          Hashtbl.find node.children c
        else
          let new_node = create_node () in
          Hashtbl.add node.children c new_node;
          new_node
      in
      aux child (i + 1)
  in
  aux trie 0
;;

let search trie word =
  let len = String.length word in
  let rec aux node i =
    if i = len then
      if node.is_end then true else false
    else
      let c = word.[i] in
      if Hashtbl.mem node.children c then
        aux (Hashtbl.find node.children c) (i + 1)
      else
        false
  in
  aux trie 0
;;

let build_trie lst =
  let trie = create_node () in
  List.iter (fun str -> insert trie str) lst;
  trie
;;

let get_max_len lst =
  let rec aux l m =
    match l with
    | [] -> m
    | hd::rest -> aux rest (max (String.length hd) m)
  in
  aux lst 0
;;

let count_possible trie max_len design =
  let len = String.length design in
  let dp = Array.make (len + 1) (0) in
  dp.(0) <- 1;
  for i = 0 to len do
    for j = (max 0 (i - max_len)) to i - 1 do
      let sub = String.sub design j (i - j) in
      if search trie (sub) then (
        dp.(i) <- dp.(i) + dp.(j);
      )
    done
  done;

  dp.(len)
;;

let count f =
  let (patterns, designs) = prepare f in
  let max_pat = get_max_len patterns in
  let trie = build_trie patterns in
  let get_count = count_possible trie max_pat in
  List.length (List.filter (fun d -> get_count d > 0) designs)
;;

let result () = count file;;

