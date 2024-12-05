let file = "./day_5/input/input1.txt";;

let check (a, b) seq =
  let arr_sec = Array.of_list seq  in
  let re = ref true in
  let idx = ref 0 in
  while !idx < (Array.length arr_sec) && !re do
    let jdx = ref (!idx + 1) in
    while !jdx < (Array.length arr_sec) && !re do
      if b = Array.get arr_sec !idx && a = Array.get arr_sec !jdx then re := false;
      incr jdx
    done;
    incr idx;
  done;
  !re;;

let count_pages f =
  In_channel.with_open_bin f In_channel.input_all 
  |> String.split_on_char '\n'
  |> List.fold_left (fun (pairs, seqs) a -> 
      if String.contains a '|' then (pairs @ [a], seqs) 
      else if String.contains a ',' then (pairs, seqs @ [a]) 
      else (pairs, seqs)
  ) ([], [])
  |> (fun (pairs, seqs) -> (
    List.map (fun pair -> String.split_on_char '|' pair) pairs
    |> List.map (fun pair -> match pair with | fst::scd::_ -> (fst,scd) | _ -> ("", ""))
    |> List.map (fun (fst, scd) -> (int_of_string fst, int_of_string scd)),
    List.map (fun seq -> String.split_on_char ',' seq) seqs
    |> List.map (fun seq -> List.map (int_of_string) seq)
  ))
  |> (fun (rules, seqs) -> (
    List.filter (fun seq -> (List.fold_left (fun res r -> if not res then false else (check r seq)) true rules)) seqs
  ))
  |> List.fold_left (fun acc lst -> acc @ [List.nth lst (List.length lst / 2)]) []
  |> List.fold_left (+) 0
;;

let result () = count_pages file;;

