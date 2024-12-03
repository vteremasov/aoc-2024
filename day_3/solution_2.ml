let input_file = "./day_3/input/input2.txt"

let read_file file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  contents;;

module Int = struct
  include Int

  let of_string_opt s =
    try Some (int_of_string s) with
    | Failure _ -> None
end

type t = {
  add : int;
  total : int;
}

let solution r str =
  Str.full_split r str
  |> List.fold_left
       (fun acc s ->
         match s with
         | Str.Text _ -> acc
         | Delim "do()" -> { acc with add = 1 }
         | Delim "don't()" -> { acc with add = 0 }
         | Delim x ->
             let r = Str.regexp {|[(,)]|} in
             let mul = Str.split r x |> List.filter_map Int.of_string_opt |> List.fold_left (fun sum x -> sum * x) 1 in
             { acc with total = acc.total + (acc.add * mul) })
       { add = 1; total = 0 }

let result () = (solution (Str.regexp {|mul([0-9]+,[0-9]+)\|do()\|don't()|}) (read_file input_file)).total;;

