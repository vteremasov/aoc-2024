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
  List.filter (fun c -> c != ".") lst
  |> List.map int_of_string
  |> List.mapi (fun i n -> i * n)
;;

let solve f =
  In_channel.with_open_bin f In_channel.input_all
  |> String.trim
  |> make_mem
  |> compact
  |> multiply
  |> List.fold_left (+) 0
;;

let result () = solve file;;

