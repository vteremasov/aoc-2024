open String;;
open List  ;;
open Lib   ;;

let file = "./day_10/input/input"

let get map (x, y) = 
  (nth map y).[x]
;;

let is_ok map el (x, y) = 
  if Lib.is_end map (x, y) then
    false
  else
    char_to_int (get map (x, y)) = el + 1
;;

let get_dirs map el (x, y) =
  let res = ref [] in
  if is_ok map el (x - 1, y) then
    res := (x - 1, y)::!res;
  if is_ok map el (x, y - 1) then
    res := (x, y - 1)::!res;
  if is_ok map el (x + 1, y) then
    res := (x + 1, y)::!res;
  if is_ok map el (x, y + 1) then
    res := (x, y + 1)::!res;

  !res
;;

let rec find_ends map el (x, y) res =
  if el = 9 then (
    [(x, y)]
  ) else (
    let dirs = get_dirs map el (x, y) in
    if List.length dirs > 0 then (
      res @ List.fold_left (fun acc dir -> acc @ (find_ends map (el + 1) dir res)) [] dirs
    )
    else
      res
  )
;;

let get_paths map =
  to_seq map
  |> Seq.mapi (
    fun y str -> 
      String.to_seq str 
      |> Seq.mapi (
        fun x chr ->
          if chr = '0' then (
            find_ends map (char_to_int chr) (x, y) []
            |> List.to_seq
          ) else 
            List.to_seq []
      )
    )
  |> Seq.flat_map Lib.nop
  |> Seq.flat_map Lib.nop
;;

let solve f =
  In_channel.with_open_bin f In_channel.input_all
  |> split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
  |> get_paths
  |> List.of_seq
  |> List.length
;;

let result () = solve file;


