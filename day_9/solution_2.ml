let file = "./day_9/input/input";;

let int_value c = int_of_char c - int_of_char '0';;

let rec make_str len str res =
  if len = 0 then
    res
  else
    str ^ (make_str (len - 1) str res)
;;

let make_mem str =
  let res = ref [] in
  let file_id = ref 0 in
  String.iteri (fun i c ->
    let items = 
      if (i + 1) mod 2 = 0 then
        List.init (int_value c) (fun _ -> ".")
      else (
        let id = string_of_int !file_id in
        incr file_id;
        List.init (int_value c) (fun _ -> id)
      )
    in
    res := List.rev_append items !res
  ) str;
  List.rev !res
;;

let swap arr i j =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp
;;

let swap_many arr i j count =
  Seq.init (count) (Lib.nop)
  |> Seq.iter (fun l -> swap arr (i + l) (j + l))
;;

let find_seq arr start = 
  let dit = arr.(start) in
  let rec aux s l =
    if s = 0 then
      (s, l + 1)
    else if arr.(s) = dit then
      aux (s - 1) (l + 1)
    else
      (s + 1, l)
  in
  aux start 0
;;

let find_space arr start count =
  let i = ref 0 in
  let c = ref 0 in
  while (!i < start) && (!c < count) do
    if arr.(!i) = "." then incr c else c := 0;
    incr i;
  done;
  if !c = count then !i - count else start
;;

let compact_ad lst =
  let arr = Array.of_list lst in
  let right = ref ((Array.length arr) - 1) in
  while !right > 0 do
    if arr.(!right) = "." then
      decr right
    else (
      let (start, count) = find_seq arr !right in
      let newStart = find_space arr start count in
      if start != newStart then swap_many arr newStart start count;
      right := !right - count
    );
  done;
  Array.to_list arr;
;;

let compact lst =
  let arr = Array.of_list lst in
  let n = Array.length arr in
  let i = ref 0 in
  let j = ref (n - 1) in
  while !i < !j do
    if arr.(!i) = "." then (
      while !i < !j && arr.(!j) = "." do
        decr j
      done;
      if !i < !j then (
        arr.(!i) <- arr.(!j);
        arr.(!j) <- ".";
        decr j
      )
    );
    incr i
  done;
  Array.to_list arr
;;

let multiply lst =
  List.map (fun c -> if c = "." then "0" else c) lst
  |> List.map int_of_string
  |> List.mapi (fun i n -> i * n)
;;

let solve f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.trim
  |> make_mem
  |> compact_ad
  |> multiply
  |> List.fold_left (+) 0
;;

let result () = solve file;;

